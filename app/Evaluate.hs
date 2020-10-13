{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Evaluate where

import Ai
import Control.Monad (join)
import Control.Parallel.Strategies (parMap, rpar)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Csv
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Vector (fromList)
import Debug.Trace
import Flow ((<|), (|>))
import GHC.Float (float2Double, int2Double)
import GHC.Generics (Generic)
import Game
import Statistics.Sample (mean, stdDev)
import System.IO (writeFile)
import System.Random (StdGen, mkStdGen, split)
import Text.Printf (printf)
import AlphaBeta (super)

evaluate =
  let results =
        ais
          |> map
            ( \(s1, ai1) ->
                ais
                  |> map
                    ( \(s2, ai2) ->
                        let (utilities, lengths) =
                              playOften
                                ( \a ->
                                    case a of
                                      Gold -> ai1
                                      Silver -> ai2
                                )
                            vUtilities = utilities |> map float2Double |> fromList
                            vLengths = lengths |> map int2Double |> fromList
                         in ( \_ ->
                                ( s1,
                                  s2,
                                  vUtilities |> mean,
                                  vUtilities |> stdDev,
                                  vLengths |> mean,
                                  vLengths |> stdDev
                                )
                            )
                    )
            )
          |> concat
          |> parMap rpar (\a -> a ())
   in let path = "evaluation/results.csv"
       in do
            ( writeFile
                path
                ( "Gold,Silver,Utility,Utility (StdDev),Ply-Depth,Ply-Depth (StdDev)\n"
                    ++ (unpack (encode (results)))
                )
              )
            putStrLn ("Written evaluation to ./" ++ path)
            putStrLn "Run `cd evaluation && pipenv run python __main__.py` to create visualizations."

ais :: [(Text, (StdGen -> Ai))]
ais =
  [ ("a Random", Ai.random),
    ("c Minimax 1", Ai.minimax 1),
    ("d Minimax 2", Ai.minimax 2),
    ("e Supermax 2", super 2),
    ("e Supermax 3", super 3)
  ]

-- | Run multiple AIs against each other.
playOften :: (Player -> StdGen -> Ai) -> ([Float], [Int])
playOften ais =
  [3]
    |> parMap rpar (\i -> play (mkStdGen i) ais [] (initial breakthru))
    |> foldl
      ( \(us, ls) (Utility u, history) ->
          (u : us, length history : ls)
      )
      ([], [])

-- | Play a game to the end. Takes a random number generator, a specification of a (possibly random-number-generator dependent) AI for each player, the history of states, and the current states. Returns the utility of player Gold and the history of states.
play :: StdGen -> (Player -> StdGen -> Ai) -> [State] -> State -> (Utility, [State])
play g ai history state@State{player = (player, _)} =
  let Game {result, utility} = breakthru
   in case utility state of
        Just f ->
          (f Gold, reverse history)
        Nothing ->
          let (g1, g2) = split g
           in play
                g2
                ai
                (state : history)
                ( (ai player g1 state)
                    |> fmap (result state)
                    |> (\a -> if a == Nothing || a == Just Nothing then traceShow state a else a)
                    |> join
                    |> fromMaybe initial_
                )

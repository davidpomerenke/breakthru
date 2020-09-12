{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Compete where

import Ai
import Control.Monad (join)
import Control.Parallel.Strategies (parMap, rseq)
import Data.Maybe (fromMaybe)
import Data.Vector (fromList)
import Debug.Trace
import Flow ((|>))
import GHC.Float (float2Double, int2Double)
import Game
import Statistics.Sample (mean, stdDev)
import System.Random (StdGen, mkStdGen, split)
import Text.Printf (printf)

compete :: IO ()
compete =
  let (utilities, lengths) = competeOften
      vUtilities = utilities |> map float2Double |> fromList
      vLengths = lengths |> map int2Double |> fromList
   in do
        putStrLn
          ( "Utility  : "
              ++ (vUtilities |> mean |> show)
              ++ ("±" ++ (vUtilities |> stdDev |> show))
          )
        putStrLn
          ( "Ply-depth: " ++ (vLengths |> mean |> printf "%.2f")
              ++ ("±" ++ (vLengths |> stdDev |> printf "%.2f"))
          )

-- | Run multiple AIs against each other.
competeOften :: ([Float], [Int])
competeOften =
  [1 .. 1024]
    |> parMap
      rseq
      ( \i ->
          play
            (mkStdGen i)
            ( \a ->
                case a of
                  Gold -> Ai.max
                  Silver -> Ai.max
            )
            []
            (initial breakthru)
      )
    |> foldl
      ( \(us, ls) (Utility u, history) ->
          (u : us, length history : ls)
      )
      ([], [])

-- | Play a game to the end. Takes a random number generator, a specification of a (possibly random-number-generator dependent) AI for each player, the history of states, and the current states. Returns the utility of player Gold and the history of states.
play :: StdGen -> (Player -> StdGen -> Ai) -> [State] -> State -> (Utility, [State])
play g ai history state =
  let Game {initial, actions, result, utility} = breakthru
   in case utility state of
        Just f ->
          (f Gold, reverse history)
        Nothing ->
          let (g1, g2) = split g
           in play
                g2
                ai
                (state : history)
                ( (ai (player state) g1 state)
                    |> fmap (result state)
                    |> (\a -> if a == Nothing || a == Just Nothing then traceShow state a else a)
                    |> join
                    |> fromMaybe initial_
                )

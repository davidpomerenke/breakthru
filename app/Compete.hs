{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Compete (compete) where

import Ai (Ai (..), move, random)
import Control.Parallel.Strategies
import Data.Vector (fromList)
import Flow ((|>))
import GHC.Float (float2Double, int2Double)
import Game (Game (..), Player (..), State (..), Utility (..), breakthru)
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
          ( "Utility: "
              ++ (vUtilities |> mean |> show)
              ++ ("±" ++ (vUtilities |> stdDev |> show))
          )
        putStrLn
          ( "Length : " ++ (vLengths |> mean |> printf "%.2f")
              ++ ("±" ++ (vLengths |> stdDev |> printf "%.2f"))
          )

-- | Run multiple AIs against each other.
competeOften :: ([Float], [Int])
competeOften =
  [1 .. 256]
    |> parMap rseq
      ( \i ->
          play
            (mkStdGen i)
            ( \a ->
                case a of
                  Gold -> random
                  Silver -> random
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
          (f (player state), reverse history)
        Nothing ->
          let (g1, g2) = split g
           in play g2 ai (state : history) (ai (player state) g1 state)

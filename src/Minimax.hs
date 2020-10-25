{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Minimax where

import Control.Parallel.Strategies
import Data.Maybe
import Debug.Trace
import Flow
import Game
import Helpers
import System.Random

parallelMap :: (a -> b) -> [a] -> [b]
parallelMap = parMap rpar

-- | Minimax search. Return the best action for the player in the given state. Runtime complexity O(b^d)
minimax :: Integer -> StdGen -> State -> Maybe Action
minimax depth g state@State {player} =
  (actions breakthru) state
    |> parallelMap
      ( \action ->
          (result breakthru) state action
            |> fmap (\result -> (action, utilityOfPlayer player (innerMiniMax (depth - 1) result)))
      )
    |> catMaybes
    |> (\a -> traceShow (map snd a) a)
    |> randomBest g player
    |> fmap fst

-- | Returns the best utility for the respective player, expressed in terms of the utility of player Gold.
innerMiniMax :: Integer -> State -> Utility
innerMiniMax depth state@State {player} =
  case (utility breakthru) state of
    Just u -> u
    Nothing
      | depth <= 0 -> heuristic state
      | otherwise ->
        childStates state
          |> map (innerMiniMax (depth - 1))
          |> relativeMax player (utilityOfPlayer player (Utility (-1 / 0)))
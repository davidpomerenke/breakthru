{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Helpers where

import Data.List.Safe ((!!))
import Flow ((|>))
import Game
import System.Random (Random (randomR), StdGen)
import Prelude hiding (foldl1, max, min, (!!))

-- HELPERS FOR THE AI

-- | Type for AIs. There are three implementations of this type: `random`, `minimax`, and `alphaBeta`.
type Ai = State -> Maybe Action

-- RANDOM AI

-- | Select a random element from a list.
randomEl :: StdGen -> [a] -> Maybe a
randomEl g l = l !! (randomR (0, length l - 1) g |> fst)

-- | AI that selects random moves.
random :: StdGen -> Ai
random g state = randomEl g ((actions breakthru) state)

-- UTILITY

-- | The utility is usually given in terms of player Gold. This converts it to any player, by simply negating it.
utilityOfPlayer :: Player -> Utility -> Utility
utilityOfPlayer player (Utility u) = case player of
  Gold -> Utility u
  Silver -> Utility (- u)
  
-- HEURISTIC

-- | Maximum size of the fleet of either player.
maxFleet :: Player -> Int
maxFleet player =
  case player of
    Gold -> 12
    Silver -> 20

-- | Heuristic value for a state, in terms of player Gold's utility. Based on the number of ships.
heuristic :: State -> Utility
heuristic state =
  let fleet1 = fromIntegral (length (fleetOfPlayer Gold state))
      maxFleet1 = fromIntegral (maxFleet Gold)
      fleet2 = fromIntegral (length (fleetOfPlayer Silver state))
      maxFleet2 = fromIntegral (maxFleet Silver)
   in Utility (fleet1 / maxFleet1 - fleet2 / maxFleet2)

-- MAXIMIZATION

type Selector a = a -> [a] -> a

-- | Select an elements from a list with a custom comparator.
argF :: Ord b => (b -> b -> Bool) -> (a -> b) -> Selector a
argF comp f x = foldl (\a b -> if (f a) `comp` (f b) then a else b) x

argMax :: Ord b => (a -> b) -> Selector a
argMax = argF (>=)

max :: Ord a => Selector a
max = argMax id

argMin :: Ord b => (a -> b) -> Selector a
argMin = argF (<=)

min :: Ord a => Selector a
min = argMin id

-- | Player Gold maximizes, player Silver minimizes. Helper function for this fact.
comp :: Ord a => Player -> a -> a -> Bool
comp player = case player of
  Gold -> (>)
  Silver -> (<)

-- | Depending on the player, maximize or minimize.
relativeMax :: Ord a => Player -> Selector a
relativeMax player = argF (comp player) id

-- RANDOM MAXIMIZATION

-- | Aggregate the best actions and their corresponding utility from a list of action-utility tuples.
aggBest :: Player -> [(Action, Utility)] -> ([Action], Utility)
aggBest player actions =
  let comp_ = comp player
   in foldl
        ( \(bestActions, bestValue) (action, value) ->
            if
                | value `comp_` bestValue -> ([action], value)
                | value == bestValue -> (action : bestActions, bestValue)
                | otherwise -> (bestActions, bestValue)
        )
        ([], utilityOfPlayer player (Utility (-1 / 0)))
        actions

-- | Randomly selects one of the best actions and the corresponding utility from a list of action-utility tuples.
randomBest :: StdGen -> Player -> [(Action, Utility)] -> Maybe (Action, Utility)
randomBest g player actions =
  let (bestActions, f) = aggBest player actions
   in randomEl g bestActions |> fmap (\el -> (el, f))
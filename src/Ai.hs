{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ai where

import Control.Monad
import Control.Monad.ST
import Control.Parallel.Strategies
import Data.List.Safe
import Data.Maybe
import Debug.Trace
import Flow
import Game
import System.Random
import Prelude hiding (foldl1, max, min, (!!))
import qualified Prelude

type Ai = State -> Maybe Action

randomEl :: StdGen -> [a] -> Maybe a
randomEl g l = l !! (randomR (0, length l - 1) g |> fst)

random :: StdGen -> Ai
random g state = randomEl g ((actions breakthru) state)

maxFleet :: Player -> Int
maxFleet player =
  case player of
    Gold -> 12
    Silver -> 20

heuristic :: State -> Player -> Utility
heuristic state player =
  let fleet1 = fromIntegral (length (fleetOfPlayer player state))
      maxFleet1 = fromIntegral (maxFleet player)
      fleet2 = fromIntegral (length (fleetOfPlayer (other player) state))
      maxFleet2 = fromIntegral (maxFleet (other player))
   in Utility (fleet1 / maxFleet1 - fleet2 / maxFleet2)

-- | Aggregate the best actions and their corresponding utility from a list of action-utility tuples.
aggBest :: Player -> [(Action, Player -> Utility)] -> ([Action], Player -> Utility)
aggBest player actions =
  foldl
    ( \(bestActions, bestValue) (action, value) ->
        if
            | value player > bestValue player -> ([action], value)
            | value player == bestValue player -> (action : bestActions, bestValue)
            | otherwise -> (bestActions, bestValue)
    )
    ([], \_ -> Utility (-1 / 0))
    actions

-- | Randomly selects one of the best actions and the corresponding utility from a list of action-utility tuples.
randomBest :: StdGen -> Player -> [(Action, Player -> Utility)] -> Maybe (Action, Player -> Utility)
randomBest g player actions =
  let (bestActions, f) = aggBest player actions
   in randomEl g bestActions |> fmap (\el -> (el, f))

argF :: Ord b => (b -> b -> Bool) -> (a -> b) -> a -> [a] -> a
argF comp f x = foldl (\a b -> if (f a) `comp` (f b) then a else b) x

argMax :: Ord b => (a -> b) -> a -> [a] -> a
argMax = argF (>=)

max :: Ord a => a -> [a] -> a
max x = argMax id x

argMin :: Ord b => (a -> b) -> a -> [a] -> a
argMin = argF (<=)

min :: Ord a => a -> [a] -> a
min x = argMin id x

minimax :: Integer -> StdGen -> State -> Maybe Action
minimax depth g state@State {player = (player, _)} =
  (actions breakthru) state
    |> parMap
      rpar
      ( \action ->
          (result breakthru) state action
            |> fmap (\result -> (action, innerMiniMax (depth - 1) result))
      )
    |> catMaybes
    |> randomBest g player
    |> fmap fst

innerMiniMax :: Integer -> State -> (Player -> Utility)
innerMiniMax depth state@State {player = (player, _)} =
  (utility breakthru) state
    |> fromMaybe
      ( if
            | depth <= 0 -> (heuristic state)
            | otherwise ->
              childStates state
                |> map (innerMiniMax (depth - 1))
                |> argMax (apply player) (\_ -> Utility 0)
      )

startBounds :: Player -> Utility
startBounds _ = Utility (1 / 0)

alphaBeta :: Integer -> StdGen -> State -> Maybe Action
alphaBeta depth _ state@State {player = (player, _)} =
  case ((actions breakthru) state) of
    a : rest ->
      Just <| fst <| innerAlphaBeta depth state a rest (\_ -> Utility (1 / 0))
    [] -> Nothing

innerAlphaBeta :: Integer -> State -> Action -> [Action] -> (Player -> Utility) -> (Action, Player -> Utility)
innerAlphaBeta depth state@State {player = (player, _)} action rest bounds =
  let u =
        case (result breakthru) state action of
          Just result_ ->
            (utility breakthru) result_
              |> fromMaybe
                ( if
                      | depth <= 0 -> heuristic result_
                      | otherwise ->
                        case (actions breakthru) result_ of
                          a : rest -> snd <| innerAlphaBeta (depth - 1) result_ a rest bounds
                          [] -> \_ -> Utility 0
                )
          Nothing -> \_ -> Utility (-1 / 0)
      invertedU = let Utility u_ = u player in Utility (- u_)
      newBounds p =
        if p == player
          then bounds player
          else Prelude.min (bounds (other player)) invertedU
   in case rest of
        a : rest ->
          let nextU = innerAlphaBeta depth state a rest newBounds
           -- traceShow (replicate (20 - 4 * fromInteger depth) ' ' ++ show (length rest, u player >= bounds player))
           in if u player >= bounds player
                then (action, u) -- prune
                else argMax (apply player . snd) (action, u) [nextU] -- consider next action(s)
        [] -> (action, u)

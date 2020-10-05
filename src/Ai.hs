{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ai where

import Control.Monad (join)
import Control.Parallel.Strategies (parMap, rpar)
import Data.List.Safe (foldl1, (!!))
import Data.Maybe (catMaybes, fromMaybe)
import Debug.Trace
import Flow ((|>))
import Game
import System.Random (StdGen, randomR, split)
import Prelude hiding (foldl1, (!!))

type Ai = State -> Maybe Action

random :: StdGen -> Ai
random g state =
  let Game {actions, result} = breakthru
      actions_ = actions state
      i = (randomR (0, length actions_ - 1) g |> fst)
   in (actions_ !! i)

maxLength :: Player -> Int
maxLength player =
  case player of
    Gold -> 12
    Silver -> 20 

heuristic :: State -> Player -> Utility
heuristic state player =
  let fleet1 = fromIntegral (length (fleetOfPlayer player state))
      maxFleet1 = fromIntegral (maxLength player)
      fleet2 = fromIntegral (length (fleetOfPlayer (other player) state))
      maxFleet2 = fromIntegral (maxLength (other player))
   in Utility (fleet1 / maxFleet1 - fleet2 / maxFleet2)

max :: StdGen -> Ai
max g state@State {player = (player, _)} =
  let Game {actions, result, utility} = breakthru
      bestActions =
        actions state
          |> map (\action -> result state action |> fmap (\result -> (action, result)))
          |> catMaybes
          |> map
            ( \(action, result) ->
                ( action,
                  utility result
                    |> fmap (\f -> let Utility u = f player in 1000 * u)
                    |> fromMaybe (let Utility u = heuristic result player in u)
                )
            )
          |> foldl
            ( \(bestActions, bestValue) (action, value) ->
                if
                    | value > bestValue -> ([action], value)
                    | value == bestValue -> (action : bestActions, bestValue)
                    | otherwise -> (bestActions, bestValue)
            )
            ([], -1000)
          |> fst
      i = (randomR (0, length bestActions - 1) g |> fst)
   in bestActions !! i

minimax :: Int -> StdGen -> Ai
minimax depth g state = innerMiniMax depth g state |> fmap fst

innerMiniMax :: Int -> StdGen -> State -> Maybe (Action, Player -> Utility)
innerMiniMax depth g state@State {player = (player, _)} =
  let (g1, g2) = split g
      map_ = map --if depth >= 2 then parMap rpar else map
      Game {actions, result, utility} = breakthru
      (bestActions, f) =
        actions state
          |> map_
            ( \action ->
                result state action
                  |> fmap
                    ( \result ->
                        ( action,
                          utility result
                            |> fromMaybe
                              ( if depth > 1
                                  then
                                    innerMiniMax (depth - 1) g2 result
                                      |> fmap snd
                                      |> fromMaybe (traceShow (state, action, result) heuristic result)
                                  else heuristic result
                              )
                        )
                    )
            )
          |> catMaybes
          |> foldl
            ( \(bestActions, bestValue) (action, value) ->
                if
                    | value player > bestValue player -> ([action], value)
                    | value player == bestValue player -> (action : bestActions, bestValue)
                    | otherwise -> (bestActions, bestValue)
            )
            ([], \_ -> Utility (-1 / 0))
      i = (randomR (0, length bestActions - 1) g1 |> fst)
   in bestActions !! i |> fmap (\bestAction -> (bestAction, f))
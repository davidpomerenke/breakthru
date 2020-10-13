{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ai where

import Control.Monad (join)
import Control.Parallel.Strategies (parMap, rpar)
import Data.List.Safe ((!!))
import Data.Maybe (catMaybes, fromMaybe)
import Debug.Trace
import Flow ((|>))
import Game
import System.Random (StdGen, randomR, split)
import Prelude hiding (foldl1, (!!))

type Ai = State -> Maybe Action

random :: StdGen -> Ai
random g state =
  let actions_ = (actions breakthru) state
      i = (randomR (0, length actions_ - 1) g |> fst)
   in (actions_ !! i)

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

-- | Randomly selects one of the best actions and the corresponding utility from a list of action-utility tuples.
randomBest :: StdGen -> Player -> [(Action, Player -> Utility)] -> Maybe (Action, Player -> Utility)
randomBest g player actions =
  let (bestActions, f) =
        foldl
          ( \(bestActions, bestValue) (action, value) ->
              if
                  | value player > bestValue player -> ([action], value)
                  | value player == bestValue player -> (action : bestActions, bestValue)
                  | otherwise -> (bestActions, bestValue)
          )
          ([], \_ -> Utility (-1 / 0))
          actions
      i = (randomR (0, length bestActions - 1) g |> fst)
   in bestActions !! i |> fmap (\randomBest -> (randomBest, f))

minimax :: Int -> StdGen -> Ai
minimax depth g state = innerMiniMax depth g state |> fmap fst

innerMiniMax :: Int -> StdGen -> State -> Maybe (Action, Player -> Utility)
innerMiniMax depth g state@State {player = (player, _)} =
  let (g1, g2) = split g
      map_ = if depth >= 3 then parMap rpar else map
      Game {actions, result, utility} = breakthru
   in actions state
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
                                    |> fromMaybe (heuristic result)
                                else heuristic result
                            )
                      )
                  )
          )
        |> catMaybes
        |> randomBest g1 player

-- Assumption: Zero-sum game.
alphaBeta :: Int -> StdGen -> Ai
alphaBeta depth g state = innerAlphaBeta (\_ -> Utility (1 / 0)) depth g state ((actions breakthru) state) |> fmap fst

innerAlphaBeta :: (Player -> Utility) -> Int -> StdGen -> State -> [Action] -> Maybe (Action, Player -> Utility)
innerAlphaBeta bounds depth g state@State {player = (player, _)} relActions =
  let (g1, g2) = split g
   in case relActions of
        a : rest ->
          case (result breakthru) state a of
            Just result_ ->
              case innerAlphaBeta bounds {- todo -} (depth - 1) g2 result_ ((actions breakthru) result_) of
                Just (a, ua)
                  | ua player <= (bounds player) -> Just (a, ua) --prune
                  | otherwise -> case innerAlphaBeta bounds depth g1 state rest of -- look at other nodes
                    Just (rest, urest)
                      | ua player == urest player -> randomBest g1 player [(a, ua), (rest, urest)]
                      | ua player > urest player -> Just (a, ua)
                      | otherwise -> Just (rest, urest)
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
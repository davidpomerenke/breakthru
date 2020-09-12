{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ai where

import Control.Monad (join)
import Data.List.Safe (foldl1, (!!))
import Data.Maybe (catMaybes, fromMaybe)
import Debug.Trace
import Flow ((|>))
import Game
import System.Random (StdGen, mkStdGen, randomR)
import Prelude hiding (foldl1, (!!))

type Ai = State -> Maybe Action

random :: StdGen -> Ai
random g state =
  let Game {actions, result} = breakthru
      actions_ = actions state
      i = (randomR (0, length actions_ - 1) g |> fst)
   in (actions_ !! i)

heuristic :: Player -> State -> Utility
heuristic player state =
  Utility
    ( fromIntegral
        ( length (fleetOfPlayer player state)
            - length (fleetOfPlayer (other player) state)
        )
    )

max :: StdGen -> Ai
max g state@State {gold, player} =
  let Game {actions, result, utility} = breakthru
   in actions state
        |> map (\action -> result state action |> fmap (\result -> (action, result)))
        |> catMaybes
        |> map
          ( \(action, result) ->
              ( action,
                utility state
                  |> fmap (\f -> let Utility u = f player in 1000 * u)
                  |> fromMaybe (let Utility u = heuristic player result in u)
              )
          )
        |> foldl
          ( \(bestActions, bestValue) (action, value) ->
              if
                  | value > bestValue ->
                    ([action], value)
                  | value == bestValue ->
                    (action : bestActions, bestValue)
                  | otherwise ->
                    (bestActions, bestValue)
          )
          ([], -1000)
        |> fst
        |> ( \l ->
               let i = (randomR (0, length l - 1) g |> fst)
                in l !! i
           )

min :: StdGen -> Ai
min g state@State {gold, player} =
  let Game {actions, result, utility} = breakthru
   in actions state
        |> map (\action -> result state action |> fmap (\result -> (action, result)))
        |> catMaybes
        |> map
          ( \(action, result) ->
              ( action,
                utility state
                  |> fmap (\f -> let Utility u = f player in -1000 * u)
                  |> fromMaybe (let Utility u = heuristic player result in u)
              )
          )
        |> foldl
          ( \(bestActions, bestValue) (action, value) ->
              if
                  | value < bestValue -> ([action], value)
                  | value == bestValue -> (action : bestActions, bestValue)
                  | otherwise -> (bestActions, bestValue)
          )
          ([], 1000)
        |> fst
        |> ( \l ->
               let i = (randomR (0, length l - 1) g |> fst)
                in l !! i
           )
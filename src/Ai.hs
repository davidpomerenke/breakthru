{-# LANGUAGE NamedFieldPuns #-}

module Ai (move, random) where

import Data.Maybe (fromMaybe)
import Flow ((|>))
import Game (Coordinate (..), Game (..), Player (..), State (..), breakthru)
import System.Random (StdGen, mkStdGen, randomR)

-- | Move of the AI player.
move :: State -> State
move state =
  let Game {actions, result} = breakthru
   in case actions state of
        h : _ -> result state h |> fromMaybe state
        [] -> state

random :: StdGen -> State -> State
random g state =
  let Game {actions, result} = breakthru
      actions_ = actions state
      i = (randomR (0, length actions_ - 1) g |> fst)
   in if length actions_ > 0
        then
          actions_ !! i
            |> result state
            |> fromMaybe state
        else state

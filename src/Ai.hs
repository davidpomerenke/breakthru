{-# LANGUAGE NamedFieldPuns #-}

module Ai (move, random, Ai (..)) where

import Data.Maybe (fromMaybe)
import Flow ((|>))
import Game (Coordinate (..), Game (..), Player (..), State (..), breakthru)
import System.Random (StdGen, mkStdGen, randomR)

type Ai = State -> State

-- | Move of the AI player.
move :: Ai
move state =
  let Game {actions, result} = breakthru
   in case actions state of
        h : _ -> result state h |> fromMaybe state
        [] -> state

random :: StdGen -> Ai
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

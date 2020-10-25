{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module AlphaBeta where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List.Safe
import Data.Maybe
import Debug.Trace
import Flow
import Game
import Helpers
import System.Random

-- ALPHA-BETA

-- | Perform Maximin search, optimized by Alpha-Beta pruning. In theory, this decreases runtime complexity from O(b^d) to O(~b^(d/2)), which is very significant.
alphaBeta :: Integer -> StdGen -> State -> Maybe Action
alphaBeta depth _ state =
  let transpositions_ = transpositions (depth - 1) state in
  case orderedMoves transpositions_ state of
    a : rest ->
      let (action, _, tt) = innerAlphaBeta depth (\_ -> Utility (1 / 0)) transpositions_ state a rest
       in traceShow ("size of the tt" ++ show (HashMap.size tt)) (Just action)
    [] -> Nothing

-- | Upper bounds for each player. (Corresponds to Alpha [Gold] and Beta [Silver], but both bounds are expressed in positive numbers. I believe the algorithm is a bit more comprehensible this way, though of course it is still very incomprehensible.)
type Bounds = (Player -> Utility)

-- | The upper bounds are set to Infinity at the start, which means that the values of the players are not constrained.
startBounds :: Player -> Utility
startBounds _ = Utility (1 / 0)

-- | Evaluates as many actions as necessary given a state and an upper bound for each player. Updates the transposition table. 
-- | Internally works by inspecting the first action in the list of actions (with `alphaBetaBranch`) and updating and checking the bounds. Then, if necessary, calls itself on the rest of the list of the actions (where, recusrively, the next action action will be checked.)
innerAlphaBeta ::
  Integer ->
  Bounds ->
  TranspositionTable ->
  State ->
  Action ->
  [Action] ->
  (Action, Utility, TranspositionTable)
innerAlphaBeta depth bounds transpositions state@State {player} action rest =
  let (u, transpositions1) = alphaBetaBranch depth bounds transpositions state action
      newBounds p
        | p == player = bounds player
        | otherwise = Prelude.min (bounds (other player)) (utilityOfPlayer (other player) u)
      transpositions2 =
        transpositions1
          |> HashMap.insertWith (\new old -> argMax fst old [new]) state (depth, let Utility float = u in float)
   in case rest of
        h : rest
          | utilityOfPlayer player u >= bounds player -> (action, u, transpositions2) -- prune
          | otherwise ->
            let (nextAction, nextU, transpositions3) =
                  innerAlphaBeta depth newBounds transpositions2 state h rest
                (betterAction, betterU) = argMax (utilityOfPlayer player . snd) (action, u) [(nextAction, nextU)]
             in (betterAction, betterU, transpositions3) -- consider next action(s)
        [] -> (action, u, transpositions2)

-- | Determines the value of a single action given a state, by going down the branch. Also updates the transposition table. The returned utility value always refers to the Golden player.
alphaBetaBranch :: Integer -> Bounds -> TranspositionTable -> State -> Action -> (Utility, TranspositionTable)
alphaBetaBranch depth bounds transpositions state@State {player} action =
  case (result breakthru) state action of
    Just result ->
      case (utility breakthru) result of
        Just u -> (u, transpositions)
        Nothing ->
          let (lookupDepth, lookupU) = HashMap.lookup result transpositions |> fromMaybe (-1, 0)
           in if
                  | depth <= 0 -> (heuristic result, transpositions)
                  | lookupDepth >= depth -> (Utility lookupU, transpositions)
                  | otherwise ->
                    case orderedMoves transpositions result of
                      a : rest ->
                        let (_, u, t) =
                              innerAlphaBeta (depth - 1) bounds transpositions result a rest
                         in (u, t)
                      [] -> (Utility 0, transpositions)
    Nothing -> (utilityOfPlayer player (Utility (-1 / 0)), transpositions)

-- HELPERS FOR ITERATIVE DEEPENING + TRANSPOSITION TABLE

type TranspositionTable = HashMap State (Integer, Float)

-- | Transposition tables created by iterative deepening. The values of each step of depth n are reused for the next step of depth n+1. Output is a transposition table, which can be used in the final call of the alpha-beta search.
transpositions :: Integer -> State -> TranspositionTable
transpositions depth state
  | depth <= 0 = HashMap.empty
  | otherwise = case ((actions breakthru) state) of
    a : rest ->
      let (_, _, tt) =
            (innerAlphaBeta depth (\_ -> Utility (1 / 0)) (transpositions (depth - 1) state) state a rest)
       in tt
    [] -> HashMap.empty

-- | Order a list of actions by their utility for the current player. The move ordering uses the transposition table as a basis.
orderedMoves :: TranspositionTable -> State -> [Action]
orderedMoves transpositions state@State {player} =
  sortOn
    ( \action ->
        (result breakthru) state action
          |> fmap
            ( \k ->
                HashMap.lookup k transpositions |> fmap snd
                  |> fmap (utilityOfPlayer player . Utility)
            )
          |> join
          |> fromMaybe (Utility 0)
    )
    ((actions breakthru) state)
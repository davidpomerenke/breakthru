{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
  ( Game (..),
    State,
    Coordinate (..),
    breakthru,
    move,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.List ((\\))
import Flow ((|>))
import GHC.Generics (Generic)

-- | Breakthru player.
data Player = Gold | Silver deriving (Show, Generic)

-- | Position of a ship.
data Coordinate = Coordinate {x :: Int, y :: Int}
  deriving (Eq, Show, Generic)

-- | All ship coordinates of a player.
type Fleet = [Coordinate]

-- | Type for the breakthru game state.
data State = State
  { lastPlayer :: Maybe Player,
    player :: Player,
    gold :: [Coordinate],
    silver :: [Coordinate]
  }
  deriving (Show, Generic)

-- | Game action. Consists of the original and the new coordinates of the moved ship.
type Action = (Coordinate, Coordinate)

-- | Utility of a player.
data Utility = Utility Float

-- | Formal type containing any game.
data Game = Game
  { initial :: State,
    action :: State -> [Action],
    result :: State -> Action -> State,
    heuristic :: State -> Utility
  }

-- | Fleet of a player given a game state.
fleetOfPlayer :: Player -> State -> Fleet
fleetOfPlayer player (State {gold = gold, silver = silver}) =
  case player of
    Gold ->
      gold
    Silver ->
      silver

-- | Moves from a specific origin given a game state.
moves :: Coordinate -> State -> [Coordinate]
moves (Coordinate {x, y}) (State {gold, silver}) =
  let occupied = gold ++ silver
   in (([1 .. 11] |> map (\x -> Coordinate {x, y})) \\ occupied)
        ++ (([1 .. 11] |> map (\y -> Coordinate {x, y})) \\ occupied)

-- | Formal rule specification of the breakthru game.
breakthru :: [Coordinate] -> [Coordinate] -> Maybe Game
breakthru goldenFleet silverFleet =
  Just
    ( Game
        { initial =
            State
              { lastPlayer = Nothing,
                player = Gold,
                gold = goldenFleet,
                silver = silverFleet
              },
          action =
            \(state@(State {player})) ->
              fleetOfPlayer player state
                |> concatMap
                  ( \pos ->
                      moves pos state
                        |> map (\move -> (pos, move))
                  ),
          result = \state action -> state,
          heuristic = \state -> Utility 0
        }
    )

-- | Move of the AI player.
move :: State -> State
move state =
  State
    { lastPlayer = Just Silver,
      player = Gold,
      gold = [],
      silver = []
    }

-- Automatically create relevant JSON instances for communicating with the Elm frontend.

instance FromJSON Coordinate

instance ToJSON Coordinate

instance FromJSON State

instance ToJSON State

instance FromJSON Player

instance ToJSON Player
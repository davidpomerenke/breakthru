{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
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
import Data.Maybe (fromMaybe)
import Flow ((<|), (|>))
import GHC.Generics (Generic)

-- | Breakthru player.
data Player = Gold | Silver deriving (Eq, Show, Generic)

-- | Position of a ship.
data Coordinate = Coordinate {x :: Int, y :: Int}
  deriving (Eq, Show, Generic)

-- | All ship coordinates of a player.
type Fleet = [Coordinate]

-- | Type for the breakthru game state.
data State = State
  { lastPlayer :: Maybe Player,
    player :: Player,
    gold :: (Maybe Coordinate, [Coordinate]),
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
    result :: State -> Action -> Maybe State,
    utility :: State -> Maybe (Player -> Utility),
    heuristic :: State -> Player -> Utility
  }

-- | Returns the other player.
other :: Player -> Player
other p = case p of
  Gold -> Silver
  Silver -> Gold

mapSwitched :: (State -> State) -> (State -> State)
mapSwitched f = f

-- | Fleet of a player given a game state.
fleetOfPlayer :: Player -> State -> Fleet
fleetOfPlayer player (State {gold = (flagship, rest), silver}) =
  case player of
    Gold ->
      (flagship |> fmap (\a -> [a]) |> fromMaybe []) ++ rest
    Silver ->
      silver

occupied :: State -> [Coordinate]
occupied state = (fleetOfPlayer Gold state) ++ (fleetOfPlayer Silver state)

-- | Moves from a specific origin given a game state.
moves :: Coordinate -> State -> [Coordinate]
moves (Coordinate {x, y}) state =
  (([1 .. 11] |> map (\x -> Coordinate {x, y})) \\ occupied state)
    ++ (([1 .. 11] |> map (\y -> Coordinate {x, y})) \\ occupied state)

data PlayerInternal = Player | Opponent

-- | Formal rule specification of the breakthru game.
breakthru :: [Coordinate] -> [Coordinate] -> Maybe Game
breakthru goldenFleet silverFleet =
  Just
    ( Game
        { initial =
            State
              { lastPlayer = Nothing,
                player = Gold,
                gold = (Just Coordinate {x = 0, y = 4}, goldenFleet),
                silver = silverFleet
              },
          action =
            \(state@State {player}) ->
              fleetOfPlayer player state
                |> concatMap
                  ( \pos ->
                      moves pos state
                        |> map (\move -> (pos, move))
                  ),
          result =
            \state@State {player, lastPlayer, gold = (flagship, rest), silver}
             ( origin@Coordinate {x = x1, y = y1},
               end@Coordinate {x = x2, y = y2}
               ) ->
                if origin `elem` fleetOfPlayer player state
                  then
                    if
                        | (x1 == x2 || y1 == y2)
                            && end `notElem` (occupied state)
                            && (flagship /= Just origin || lastPlayer /= Just player) ->
                          -- noncapturing
                          Just
                            State
                              { lastPlayer = Just player,
                                player =
                                  if lastPlayer == Just player
                                    || flagship == Just origin
                                    then other player
                                    else player,
                                gold =
                                  case player of
                                    Gold ->
                                      if flagship == Just origin
                                        then (Just end, rest)
                                        else
                                          ( flagship,
                                            end : (filter ((/=) origin) rest)
                                          )
                                    Silver -> (flagship, rest),
                                silver = case player of
                                  Gold ->
                                    silver
                                  Silver -> end : (filter ((/=) origin) silver)
                              }
                        | lastPlayer /= Just player
                            && abs ((x2 - x1) * (y2 - y1)) == 1
                            && end `elem` (fleetOfPlayer (other player) state) ->
                          -- capturing

                          Just
                            State
                              { lastPlayer = Just player,
                                player = other player,
                                gold =
                                  case player of
                                    Gold ->
                                      if flagship == Just origin
                                        then (Just end, rest)
                                        else
                                          ( flagship,
                                            end : (filter ((/=) origin) rest)
                                          )
                                    Silver ->
                                      ( if flagship == Just end then Nothing else flagship,
                                        filter ((/=) end) rest
                                      ),
                                silver =
                                  case player of
                                    Gold -> filter ((/=) end) silver
                                    Silver -> end : (filter ((/=) origin) silver)
                              }
                        | otherwise ->
                          Nothing -- invalid end position
                  else -- no own ship on start position
                    Nothing,
          utility = \State {gold = (flagship, _)} ->
            case flagship of
              Nothing ->
                Just
                  ( \player -> case player of
                      Gold -> Utility 0
                      Silver -> Utility 1
                  )
              Just Coordinate {x, y} ->
                if x == 0 || x == 10 || y == 0 || y == 10
                  then
                    Just
                      ( \player -> case player of
                          Gold -> Utility 1
                          Silver -> Utility 0
                      )
                  else Nothing,
          heuristic = \state player -> Utility 0
        }
    )

-- | Move of the AI player.
move :: State -> State
move state =
  State
    { lastPlayer = Just Silver,
      player = Gold,
      gold = (Just Coordinate {x = 17, y = 17}, []),
      silver = []
    }

-- Automatically create relevant JSON instances for communicating with the Elm frontend.

instance FromJSON Coordinate

instance ToJSON Coordinate

instance FromJSON State

instance ToJSON State

instance FromJSON Player

instance ToJSON Player
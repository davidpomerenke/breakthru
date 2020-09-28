{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Game where

import Data.Aeson (FromJSON, ToJSON)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Flow ((<|), (|>))
import GHC.Generics (Generic)

{- TODO
- only one ship → only one move
- blocking situation = no actions → draw
-}

-- | Performs an action given a state.
move :: State -> Action -> State
move state action =
  let Game {result} = breakthru
   in fromMaybe state (result state action)

-- GAME & BREAKTHRU TYPES

-- | Formal type containing any game.
data Game = Game
  { initial :: State,
    actions :: State -> [Action],
    result :: State -> Action -> Maybe State,
    utility :: State -> Maybe (Player -> Utility)
  }

-- | Type for the breakthru game state.
data State = State
  { lastPlayer :: Maybe Player,
    player :: Player,
    gold :: (Maybe Coordinate, [Coordinate]),
    silver :: [Coordinate]
  }
  deriving (Eq, Show, Generic)

-- | Breakthru player.
data Player
  = Gold
  | Silver
  deriving (Eq, Show, Generic)

-- | Position of a ship.
data Coordinate = Coordinate {x :: Int, y :: Int}
  deriving (Eq, Show, Generic)

-- | All ship coordinates of a player.
type Fleet = [Coordinate]

-- | Game action. Consists of the original and the new coordinates of the moved ship.
type Action = (Coordinate, Coordinate)

-- | Utility of a player.
data Utility = Utility Float
  deriving (Eq, Show, Generic, Ord)

-- BREAKTHRU SPECIFICATION

-- | Formal rule specification of the breakthru game.
breakthru :: Game
breakthru =
  Game
    { initial = initial_,
      actions = actions_,
      result = result_,
      utility = utility_
    }

-- todo
initial_ :: State
initial_ =
  State
    { lastPlayer = Nothing,
      player = Gold,
      gold =
        ( Just Coordinate {x = 5, y = 5},
          [ (3, 4),
            (3, 5),
            (3, 6),
            (7, 4),
            (7, 5),
            (7, 6),
            (4, 3),
            (5, 3),
            (6, 3),
            (4, 7),
            (5, 7),
            (6, 7)
          ]
            |> map (\(x, y) -> Coordinate {x, y})
        ),
      silver =
        [ (1, 3),
          (1, 4),
          (1, 5),
          (1, 6),
          (1, 7),
          (9, 3),
          (9, 4),
          (9, 5),
          (9, 6),
          (9, 7),
          (3, 1),
          (4, 1),
          (5, 1),
          (6, 1),
          (7, 1),
          (3, 9),
          (4, 9),
          (5, 9),
          (6, 9),
          (7, 9)
        ]
          |> map (\(x, y) -> Coordinate {x, y})
    }

actions_ :: State -> [Action]
actions_ (state@State {player}) =
  case utility_ state of
    Nothing ->
      fleetOfPlayer player state
        |> concatMap
          ( \pos ->
              moves pos state
                |> map (\move -> (pos, move))
          )
    Just _ ->
      []

-- | Moves from a specific origin given a game state.
moves :: Coordinate -> State -> [Coordinate]
moves (start@Coordinate {x, y}) state@State {player, lastPlayer, gold = (flagship, _)} =
  if flagship /= Just start || lastPlayer /= Just player
    then
      ( if lastPlayer /= Just player
          then -- capturing moves

            [ Coordinate {x = x -1, y = y -1},
              Coordinate {x = x -1, y = y + 1},
              Coordinate {x = x + 1, y = y -1},
              Coordinate {x = x + 1, y = y + 1}
            ]
              |> filter (\end -> end `elem` fleetOfPlayer (other player) state)
          else []
      ) -- noncapturing moves
        ++ ( [ (([(x + 1), (x + 2) .. 10] |> map (\x -> Coordinate {x, y}))),
               (([(x - 1), (x - 2) .. 0] |> map (\x -> Coordinate {x, y}))),
               (([(y + 1), (y + 2) .. 10] |> map (\y -> Coordinate {x, y}))),
               (([(y - 1), (y - 2) .. 0] |> map (\y -> Coordinate {x, y})))
             ]
               |> concatMap (takeWhile (`notElem` (occupied state)))
           )
    else []

result_ :: State -> Action -> Maybe State
result_ =
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
          Nothing

utility_ :: State -> Maybe (Player -> Utility)
utility_ = \state@State {gold = (flagship, _)} ->
  case flagship of
    Nothing ->
      Just
        ( \player -> case player of
            Gold -> Utility (-1)
            Silver -> Utility 1
        )
    Just Coordinate {x, y} ->
      if x == 0 || x == 10 || y == 0 || y == 10
        || length (fleetOfPlayer Silver state) == 0
        then
          Just
            ( \player -> case player of
                Gold -> Utility 1
                Silver -> Utility (-1)
            )
        else Nothing

-- HELPERS

-- | Returns the other player.
other :: Player -> Player
other p = case p of
  Gold -> Silver
  Silver -> Gold

-- | Fleet of a player given a game state.
fleetOfPlayer :: Player -> State -> Fleet
fleetOfPlayer player (State {gold = (flagship, rest), silver}) =
  case player of
    Gold ->
      case flagship of
        Just a ->
          a : rest
        Nothing ->
          rest
    Silver ->
      silver

occupied :: State -> [Coordinate]
occupied state = (fleetOfPlayer Gold state) ++ (fleetOfPlayer Silver state)

-- Automatically create relevant JSON instances for communicating with the Elm frontend.

instance FromJSON Coordinate

instance ToJSON Coordinate

instance FromJSON State

instance ToJSON State

instance FromJSON Player

instance ToJSON Player
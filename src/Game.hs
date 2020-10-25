{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

module Game where

import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Data.Maybe (catMaybes, fromMaybe)
import Flow ((|>))
import GHC.Generics (Generic)

-- | Performs an action given a state.
move :: State -> Action -> State
move state action =
  let Game {result} = breakthru
   in fromMaybe state (result state action)

-- GAME & BREAKTHRU TYPES

-- | Formal type containing any zero-sum game.
data Game state action player = Game
  { initial :: state,
    actions :: state -> [action],
    result :: state -> action -> Maybe state,
    utility :: state -> Maybe Utility
  }

-- | Type for the breakthru game state.
data State = State
  { lastPlayer :: Maybe Player,
    player :: Player,
    movedPiece :: Maybe Coordinate,
    gold :: (Maybe Coordinate, [Coordinate]),
    silver :: [Coordinate]
  }
  deriving (Eq, Show, Generic, Hashable)

-- | Breakthru player.
data Player
  = Gold
  | Silver
  deriving (Eq, Show, Generic, Hashable)

-- | Position of a ship.
data Coordinate = Coordinate {x :: Int, y :: Int}
  deriving (Eq, Show, Generic, Hashable)

-- | Game action. Consists of the original and the new coordinates of the moved ship.
type Action = (Coordinate, Coordinate)

-- | Utility of a player.
data Utility = Utility Float
  deriving (Eq, Show, Generic, Ord)

-- | All ship coordinates of a player.
type Fleet = [Coordinate]



-- BREAKTHRU SPECIFICATION

-- | Formal rule specification of the breakthru game.
breakthru :: Game State Action Player
breakthru =
  Game
    { initial = initial_,
      actions = actions_,
      result = result_,
      utility = utility_
    }

-- | Initial state.
initial_ :: State
initial_ =
  State
    { lastPlayer = Nothing,
      player = Gold,
      movedPiece = Nothing,
      gold =
        ( Just Coordinate {x = 5, y = 5},
          [(3, 4), (3, 5), (3, 6), (7, 4), (7, 5), (7, 6), (4, 3), (5, 3), (6, 3), (4, 7), (5, 7), (6, 7)]
            |> map (\(x, y) -> Coordinate {x, y})
        ),
      silver =
        [(1, 3), (1, 4), (1, 5), (1, 6), (1, 7), (9, 3), (9, 4), (9, 5), (9, 6), (9, 7), (3, 1), (4, 1), (5, 1), (6, 1), (7, 1), (3, 9), (4, 9), (5, 9), (6, 9), (7, 9)]
          |> map (\(x, y) -> Coordinate {x, y})
    }

-- | Avilable actions given a state. 
actions_ :: State -> [Action]
actions_ (state@State {player}) =
  case utility_ state of
    Nothing ->
      fleetOfPlayer player state
        |> concatMap (\pos -> moves state pos |> map (\move -> (pos, move)))
    Just _ -> []

-- | Moves from a specific origin given a game state.
moves :: State -> Coordinate -> [Coordinate]
moves state@State {player, movedPiece, lastPlayer, gold = (flagship, _)} (start@Coordinate {x, y}) =
  if
      | (flagship /= Just start || lastPlayer /= Just player) && Just start /= movedPiece ->
        (nonCapturingMoves x y |> concatMap (takeWhile (`notElem` (occupied state))))
          ++ if lastPlayer /= Just player
            then capturingMoves x y |> filter (\end -> end `elem` fleetOfPlayer (other player) state)
            else []
      | otherwise -> []

-- | Capturing moves potentially available from a coordinate. (The diagonal neighbours.)
capturingMoves x y =
  [ Coordinate {x = x -1, y = y -1},
    Coordinate {x = x -1, y = y + 1},
    Coordinate {x = x + 1, y = y -1},
    Coordinate {x = x + 1, y = y + 1}
  ]

-- | Non-capturing moves potentially available from a coordinate. (The fields in the same line of each direction, until there is an obstacle.)
nonCapturingMoves x y =
  [ (([(x + 1), (x + 2) .. 10] |> map (\x -> Coordinate {x, y}))),
    (([(x - 1), (x - 2) .. 0] |> map (\x -> Coordinate {x, y}))),
    (([(y + 1), (y + 2) .. 10] |> map (\y -> Coordinate {x, y}))),
    (([(y - 1), (y - 2) .. 0] |> map (\y -> Coordinate {x, y})))
  ]

-- | New state after an action.
result_ :: State -> Action -> Maybe State
result_
  state@State {player, lastPlayer, gold = (flagship, _)}
  action@( origin@Coordinate {x = x1, y = y1},
           end@Coordinate {x = x2, y = y2}
           )
    | origin `elem` fleetOfPlayer player state =
      if
          | (x1 == x2 || y1 == y2)
              && end `notElem` (occupied state)
              && (flagship /= Just origin || lastPlayer /= Just player) ->
            nonCapturingResult state action
          | lastPlayer /= Just player
              && abs ((x2 - x1) * (y2 - y1)) == 1
              && end `elem` (fleetOfPlayer (other player) state) ->
            capturingResult state action
          | otherwise ->
            Nothing -- invalid end position
    | otherwise = Nothing -- no own ship on start position

-- | New state after an action if the action is a capture.
nonCapturingResult :: State -> Action -> Maybe State
nonCapturingResult
  state@State {player, lastPlayer, gold = (flagship, rest), silver}
  (origin, end) =
    let switchPlayers =
          lastPlayer == Just player
            || flagship == Just origin
            || length (fleetOfPlayer player state)
              <= case player of
                Gold -> 2
                Silver -> 1
     in Just
          State
            { lastPlayer = Just player,
              player =
                if switchPlayers
                  then other player
                  else player,
              movedPiece =
                if switchPlayers
                  then Nothing
                  else Just end,
              gold =
                case player of
                  Gold
                    | flagship == Just origin -> (Just end, rest)
                    | otherwise -> (flagship, end : (filter ((/=) origin) rest))
                  Silver -> (flagship, rest),
              silver = case player of
                Gold -> silver
                Silver -> end : (filter ((/=) origin) silver)
            }

-- | New state after an action if the action is not a capture.
capturingResult :: State -> Action -> Maybe State
capturingResult
  State {player, gold = (flagship, rest), silver}
  (origin, end) =
    Just
      State
        { lastPlayer = Just player,
          player = other player,
          movedPiece = Nothing,
          gold =
            case player of
              Gold
                | flagship == Just origin -> (Just end, rest)
                | otherwise -> (flagship, end : (filter ((/=) origin) rest))
              Silver ->
                ( if flagship == Just end then Nothing else flagship,
                  filter ((/=) end) rest
                ),
          silver =
            case player of
              Gold -> filter ((/=) end) silver
              Silver -> end : (filter ((/=) origin) silver)
        }

-- | Utility of a terminal state, in terms of player Gold. `Nothing` if the state is not terminal.
utility_ :: State -> Maybe Utility
utility_ = \state@State {gold = (flagship, _)} ->
  case flagship of
    Nothing -> Just (Utility (-1))
    Just Coordinate {x, y}
      | x == 0 || x == 10 || y == 0 || y == 10 || length (fleetOfPlayer Silver state) == 0 -> Just (Utility 1)
      | otherwise -> Nothing

-- HELPERS FOR GAME DEFINITION

-- | Returns the other player.
other :: Player -> Player
other p = case p of
  Gold -> Silver
  Silver -> Gold

-- | Fleet of a player given a game state.
fleetOfPlayer :: Player -> State -> Fleet
fleetOfPlayer player (State {gold = (flagship, rest), silver}) =
  case (player, flagship) of
    (Gold, Just a) -> a : rest
    (Gold, Nothing) -> rest
    (Silver, _) -> silver

occupied :: State -> [Coordinate]
occupied state = (fleetOfPlayer Gold state) ++ (fleetOfPlayer Silver state)

childStates :: State -> [State]
childStates state = actions_ state |> map (result_ state) |> catMaybes

-- Automatically create relevant JSON instances for communicating with the Elm frontend.

instance FromJSON Coordinate

instance ToJSON Coordinate

instance FromJSON State

instance ToJSON State

instance FromJSON Player

instance ToJSON Player
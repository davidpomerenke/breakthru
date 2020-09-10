module Ai (move) where

import Game (Coordinate (..), Player (..), State (..))

-- | Move of the AI player.
move :: State -> State
move state =
  State
    { lastPlayer = Just Silver,
      player = Gold,
      gold = (Just Coordinate {x = 17, y = 17}, []),
      silver = []
    }
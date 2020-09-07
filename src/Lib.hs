module Lib
  ( Board,
    move,
  )
where

type Board = [[Int]]

move :: Board -> Board
move board = board
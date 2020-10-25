module Model exposing (..)


type alias Coordinate =
    { x : Int
    , y : Int
    }

type alias Action = (Coordinate, Coordinate)

type Player
    = Gold
    | Silver


type alias State =
    { lastPlayer : Maybe Player
    , player : Player
    , movedPiece : Maybe Coordinate
    , gold : ( Maybe Coordinate, List Coordinate )
    , silver : List Coordinate
    }

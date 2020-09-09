module Model exposing (..)


type alias Coordinate =
    { x : Int
    , y : Int
    }


type Player
    = Gold
    | Silver


type alias Model =
    { lastPlayer : Maybe Player
    , player : Player
    , gold : ( Maybe Coordinate, List Coordinate )
    , silver : List Coordinate
    }
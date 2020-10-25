module Display exposing (..)

import Browser
import Http exposing (..)
import Json exposing (decodeState)
import Main exposing (..)
import Model exposing (Player(..))


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view =
            \model ->
                { title = "breakthru static display"
                , body = [ page model ]
                }
        , update = update
        , subscriptions = \_ -> Sub.none
        }



init : () -> ( Model, Cmd Msg )
init _ =
    let
        emptyState =
            { lastPlayer = Nothing
            , player = Gold
            , movedPiece = Nothing
            , gold = ( Nothing, [] )
            , silver = []
            }
    in
    ( { ai1 = Nothing
      , ai2 = Nothing
      , ai1Selected = True
      , ai2Selected = True
      , actions = []
      , selectedShip = Nothing
      , winner = Nothing
      , state = emptyState
      , lastStates = []
      }
    , getBoard
    )


getBoard : Cmd Msg
getBoard =
    Http.post
        { url = "/display"
        , body = Http.emptyBody
        , expect = Http.expectJson ShowBoardAndChill decodeState
        }

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
    ( { ai = \_ -> Nothing
      , actions = []
      , selectedShip = Nothing
      , winner = Nothing
      , state =
            { lastPlayer = Nothing
            , player = (Gold, Nothing)
            , gold = ( Nothing, [] )
            , silver = []
            } 
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

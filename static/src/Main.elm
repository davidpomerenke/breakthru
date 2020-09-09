module Main exposing (..)

import Browser
import Element exposing (..)
import Http
import Json exposing (decodeModel, encodeMove)
import Model exposing (Model, Player(..))


type Msg
    = GotBoard (Result Http.Error Model)


init : () -> ( Model, Cmd Msg )
init _ =
    let
        init_ =
            { lastPlayer = Nothing
            , player = Gold
            , gold = ( Just { x = 3, y = 3 }, [] )
            , silver = []
            }
    in
    ( init_
    , getBoard init_
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBoard r ->
            case r of
                Ok a ->
                    ( a, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = \model -> { title = "breakthru", body = [ layout [] (page model) ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }



--


getBoard move =
    Http.post
        { url = "/api"
        , body = Http.jsonBody (encodeMove move)
        , expect = Http.expectJson GotBoard decodeModel
        }


page : Model -> Element Msg
page model =
    text
        (model |> Debug.toString)

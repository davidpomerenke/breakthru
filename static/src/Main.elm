module Main exposing (..)

import Browser
import Element exposing (..)
import Http
import Json.Decode as D
import Json.Encode as E


type alias Model =
    List (List Int)


type Msg
    = NoOp
    | GotBoard (Result Http.Error Model)


init : () -> ( Model, Cmd Msg )
init _ =
    let
        board =
            [ [ 1, 2, 3 ] ]
    in
    ( board
    , getBoard board
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBoard r ->
            case r of
                Ok a ->
                    ( a, Cmd.none )

                Err _ ->
                    ( [ [ 404 ] ], Cmd.none )

        _ ->
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


getBoard board =
    Http.post
        { url = "/api"
        , body = Http.jsonBody (E.list (E.list E.int) board)
        , expect = Http.expectJson GotBoard (D.list (D.list D.int))
        }


page : Model -> Element Msg
page model =
    text
        (model
            |> List.map (List.map String.fromInt)
            |> List.concat
            |> String.concat
        )

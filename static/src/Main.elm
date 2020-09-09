module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Html exposing (Html)
import Html.Attributes exposing (style)
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
            , gold =
                ( Just { x = 5, y = 5 }
                , [ { x = 3, y = 4 }
                  , { x = 3, y = 5 }
                  , { x = 3, y = 6 }
                  , { x = 7, y = 4 }
                  , { x = 7, y = 5 }
                  , { x = 7, y = 6 }
                  , { x = 4, y = 3 }
                  , { x = 5, y = 3 }
                  , { x = 6, y = 3 }
                  , { x = 4, y = 7 }
                  , { x = 5, y = 7 }
                  , { x = 6, y = 7 }
                  ]
                )
            , silver =
                [ { x = 1, y = 3 }
                , { x = 1, y = 4 }
                , { x = 1, y = 5 }
                , { x = 1, y = 6 }
                , { x = 1, y = 7 }
                , { x = 9, y = 3 }
                , { x = 9, y = 4 }
                , { x = 9, y = 5 }
                , { x = 9, y = 6 }
                , { x = 9, y = 7 }
                , { x = 3, y = 1 }
                , { x = 4, y = 1 }
                , { x = 5, y = 1 }
                , { x = 6, y = 1 }
                , { x = 7, y = 1 }
                , { x = 3, y = 9 }
                , { x = 4, y = 9 }
                , { x = 5, y = 9 }
                , { x = 6, y = 9 }
                , { x = 7, y = 9 }
                ]
            }
    in
    ( init_
    , Cmd.none
      --getBoard init_
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
        , view =
            \model ->
                { title = "breakthru"
                , body = [ page model ]
                }
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


page : Model -> Html Msg
page { gold, silver } =
    let
        ( flagship, rest ) =
            gold
    in
    layout
        [ centerX
        , centerY
        , Background.color (rgba 0 0 0 0.7)
        ]
        (row
            [ htmlAttribute (style "height" "100vmin")
            , htmlAttribute (style "width" "100vmin")
            , centerX
            , centerY
            , Border.width 3
            , Border.glow (rgb 0 0 0) 20
            ]
            (List.range 0 10
                |> List.map
                    (\x ->
                        column
                            [ height fill
                            , width fill
                            ]
                            (List.range 0 10
                                |> List.map
                                    (\y ->
                                        let
                                            pos =
                                                { x = x, y = y }

                                            flagship_ =
                                                flagship == Just pos

                                            gold_ =
                                                List.member pos rest

                                            silver_ =
                                                List.member pos silver
                                        in
                                        el
                                            [ height fill
                                            , width fill
                                            , Border.color (rgb 0 0 0)
                                            , Border.width 1
                                            , Border.innerGlow (rgba 0.2 0.2 0.1 0.3) 20
                                            , Background.color (rgb 0.7 0.6 0.4)
                                            , padding 10
                                            ]
                                            (if flagship_ || gold_ || silver_ then
                                                el
                                                    [ height (fillPortion 1)
                                                    , width fill
                                                    , Border.rounded 100
                                                    , Border.glow (rgba 1 0 0 1) 10
                                                    , Border.width
                                                        (if flagship_ then
                                                            3

                                                         else
                                                            1
                                                        )
                                                    , Border.innerGlow
                                                        (if flagship_ then
                                                            rgba 0 0 0 0.8

                                                         else
                                                            rgba 0 0 0 0.5
                                                        )
                                                        10
                                                    , Background.color
                                                        (if flagship_ then
                                                            rgb 1 1 0

                                                         else if gold_ then
                                                            rgb 1 1 0

                                                         else if silver_ then
                                                            rgb 0.8 0.9 0.9

                                                         else
                                                            rgba 1 1 1 0
                                                        )
                                                    ]
                                                    (text "")

                                             else
                                                text ""
                                            )
                                    )
                            )
                    )
            )
        )

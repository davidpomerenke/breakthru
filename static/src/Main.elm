module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Html exposing (Html)
import Html.Attributes exposing (selected, style)
import Http
import Json exposing (decodeActions, decodeModel, encodeMoveAndState, encodeState)
import Model exposing (Coordinate, Player(..), State)


type alias Model =
    { actions : List ( Coordinate, Coordinate )
    , selectedShip : Maybe Coordinate
    , game : State
    }


type Msg
    = GotActions (Result Http.Error (List ( Coordinate, Coordinate )))
    | GotBoard (Result Http.Error State)
    | SelectShip (Maybe Coordinate)


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
    ( { actions = []
      , selectedShip = Nothing
      , game = init_
      }
    , getActions init_
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ game } as model) =
    case msg of
        GotActions r ->
            let
                _ =
                    Debug.log "actions" r
            in
            case r of
                Ok a ->
                    ( { model | actions = a }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GotBoard r ->
            case r of
                Ok a ->
                    ( { model | game = a, selectedShip = Nothing }, getActions a )

                Err _ ->
                    ( model, Cmd.none )

        SelectShip ship ->
            case model.selectedShip of
                Nothing ->
                    ( { model | selectedShip = ship }, Cmd.none )

                Just start ->
                    ( model
                    , case ship of
                        Just end ->
                            getState ( start, end ) game

                        Nothing ->
                            Cmd.none
                    )


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


getActions state =
    Http.post
        { url = "/actions"
        , body = Http.jsonBody (encodeState state)
        , expect = Http.expectJson GotActions decodeActions
        }


getState action state =
    Http.post
        { url = "/move"
        , body = Http.jsonBody (encodeMoveAndState ( action, state ))
        , expect = Http.expectJson GotBoard decodeModel
        }


getAiMove state =
    Http.post
        { url = "/ai-move"
        , body = Http.jsonBody (encodeState state)
        , expect = Http.expectJson GotBoard decodeModel
        }


page : Model -> Html Msg
page { game, actions, selectedShip } =
    let
        { gold, silver } =
            game

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

                                            clickable =
                                                case selectedShip of
                                                    Nothing ->
                                                        actions
                                                            |> List.map Tuple.first
                                                            |> List.member { x = x, y = y }

                                                    Just ship ->
                                                        actions
                                                            |> List.filter (\( s, _ ) -> s == ship)
                                                            |> List.map Tuple.second
                                                            |> List.member { x = x, y = y }
                                        in
                                        el
                                            ([ height fill
                                             , width fill
                                             , Border.color (rgb 0 0 0)
                                             , Border.width 1
                                             , Border.innerGlow (rgba 0.2 0.2 0.1 0.3) 20
                                             , Background.color
                                                (if clickable then
                                                    rgb 0.2 0.6 0.4

                                                 else
                                                    rgb 0.7 0.6 0.4
                                                )
                                             , padding 10
                                             ]
                                                ++ (if clickable then
                                                        [ Events.onClick (SelectShip (Just { x = x, y = y }))
                                                        , pointer
                                                        ]

                                                    else
                                                        [ Events.onClick (SelectShip Nothing) ]
                                                   )
                                            )
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

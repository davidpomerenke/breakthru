module Main exposing (Model, Msg(..), main, page, update)

import Array
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events exposing (onClick)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (style)
import Http
import Json exposing (decodeActions, decodeState, decodeUtility, encodeState, encodeStateAndMove)
import Model exposing (Action, Coordinate, Player(..), State)
import Process
import Task


aiConfig : Player -> Maybe Ai
aiConfig =
    \player ->
        case player of
            Gold ->
                Just Minimax

            Silver ->
                Nothing


pause : Float
pause =
    2000


type Ai
    = Random
    | Max
    | Minimax


type alias Model =
    { ai : Player -> Maybe Ai
    , actions : List ( Coordinate, Coordinate )
    , selectedShip : Maybe Coordinate
    , winner : Maybe String
    , state : State
    , lastStates : List State
    }


type Msg
    = GotActions (Result Http.Error (List ( Coordinate, Coordinate )))
    | GotBoard (Result Http.Error State)
    | GotUtility (Result Http.Error Float)
    | WaitedForAiMove
    | SelectShip (Maybe Coordinate)
    | ShowBoardAndChill (Result Http.Error State)
    | Back


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
    ( { ai = aiConfig
      , actions = []
      , selectedShip = Nothing
      , winner = Nothing
      , state = emptyState
      , lastStates = []
      }
    , getInit
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ state } as model) =
    case msg of
        GotActions r ->
            case r of
                Ok a ->
                    ( { model | actions = a }
                    , if a == [] then
                        getUtility state

                      else
                        Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotBoard r ->
            case r of
                Ok a ->
                    ( { model
                        | state = a
                        , lastStates = model.state :: model.lastStates
                        , selectedShip = Nothing
                      }
                    , getUtility a
                    )

                Err _ ->
                    ( model, Cmd.none )

        GotUtility u ->
            case u of
                Ok u_ ->
                    ( { model
                        | winner =
                            if u_ > 0 then
                                Just "GOLD"

                            else if u_ < 0 then
                                Just "SILVER"

                            else
                                Nothing
                      }
                    , case model.ai state.player of
                        Just _ ->
                            Process.sleep pause |> Task.perform (\_ -> WaitedForAiMove)

                        Nothing ->
                            getActions model.state
                    )

                Err _ ->
                    ( model, Cmd.none )

        SelectShip ship ->
            case model.selectedShip of
                Nothing ->
                    ( { model | selectedShip = ship }, Cmd.none )

                Just start ->
                    case ship of
                        Just end ->
                            ( { model
                                | selectedShip = Nothing
                                , actions = []
                              }
                            , getResult state ( start, end )
                            )

                        Nothing ->
                            ( { model | selectedShip = Nothing }, Cmd.none )

        WaitedForAiMove ->
            ( model
            , case aiConfig state.player of
                Just ai ->
                    getAiMove ai model.state

                Nothing ->
                    Cmd.none
            )

        ShowBoardAndChill r ->
            case r of
                Ok a ->
                    ( { model | state = a, selectedShip = Nothing }
                    , getActions a
                    )

                Err _ ->
                    ( model, Cmd.none )

        Back ->
            case model.lastStates of
                h :: rest ->
                    ( { model
                        | state = h
                        , lastStates = rest
                      }
                    , getActions h
                    )

                _ ->
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


getInit : Cmd Msg
getInit =
    Http.post
        { url = "/init"
        , body = Http.emptyBody
        , expect = Http.expectJson GotBoard decodeState
        }


getActions : State -> Cmd Msg
getActions state =
    Http.post
        { url = "/actions"
        , body = Http.jsonBody (encodeState state)
        , expect = Http.expectJson GotActions decodeActions
        }


getResult : State -> Action -> Cmd Msg
getResult state action =
    Http.post
        { url = "/result"
        , body = Http.jsonBody (encodeStateAndMove ( state, action ))
        , expect = Http.expectJson GotBoard decodeState
        }


getUtility : State -> Cmd Msg
getUtility state =
    Http.post
        { url = "/utility"
        , body = Http.jsonBody (encodeState state)
        , expect = Http.expectJson GotUtility decodeUtility
        }


getAiMove : Ai -> State -> Cmd Msg
getAiMove ai state =
    Http.post
        { url =
            "/ai/"
                ++ (case ai of
                        Random ->
                            "random"

                        Max ->
                            "max"

                        Minimax ->
                            "minimax"
                   )
        , body = Http.jsonBody (encodeState state)
        , expect = Http.expectJson GotBoard decodeState
        }


page : Model -> Html Msg
page model =
    layout
        [ centerX
        , centerY
        , Background.color (rgba 0 0 0 0.7)
        ]
        (column
            [ centerX ]
            [ row
                [ htmlAttribute (style "height" "100vmin")
                , htmlAttribute (style "width" "100vmin")
                , centerX
                , centerY
                , Border.width 3
                , Border.glow (rgb 0 0 0) 20
                ]
                ((case model.winner of
                    Nothing ->
                        none

                    Just winner_ ->
                        el
                            (([ style "position" "absolute"
                              , style "z-index" "1"
                              , style "width" "100%"
                              , style "height" "100%"
                              ]
                                |> List.map htmlAttribute
                             )
                                ++ [ Background.color (rgba 0 0 0 0.7) ]
                            )
                            (el
                                [ centerX
                                , centerY
                                , Font.size 100
                                , Font.color (rgb 1 1 1)
                                , Font.extraBold
                                ]
                                (text (winner_ ++ " WINS"))
                            )
                 )
                    :: (List.range 0 10
                            |> List.map
                                (\x ->
                                    column
                                        [ height fill
                                        , width fill
                                        ]
                                        (List.range 0 10
                                            |> List.map
                                                (\y ->
                                                    tile model x y
                                                )
                                        )
                                )
                       )
                )
            , el
                [ onClick Back
                , Background.color (rgb 1 1 1)
                , Border.rounded 10
                , padding 10
                , alignRight
                , Font.size 20
                , htmlAttribute (style "position" "absolute")
                , htmlAttribute (style "bottom" "15px")
                , htmlAttribute (style "right" "-45px")
                , pointer
                ]
                (text "↩")
            ]
        )


tile : Model -> Int -> Int -> Element Msg
tile { actions, state, lastStates, selectedShip } x y =
    let
        { gold, silver } =
            state

        ( flagship, rest ) =
            gold

        currentShips =
            silver
                ++ (gold |> Tuple.second)
                ++ (case gold |> Tuple.first of
                        Just a ->
                            [ a ]

                        _ ->
                            []
                   )

        lastShips =
            case lastStates of
                h :: _ ->
                    h.silver
                        ++ (h.gold |> Tuple.second)
                        ++ (case h.gold |> Tuple.first of
                                Just a ->
                                    [ a ]

                                _ ->
                                    []
                           )

                _ ->
                    []

        beforeLastShips =
            case lastStates of
                _ :: h2 :: _ ->
                    h2.silver
                        ++ (h2.gold |> Tuple.second)
                        ++ (case h2.gold |> Tuple.first of
                                Just a ->
                                    [ a ]

                                _ ->
                                    []
                           )

                _ ->
                    []

        pos =
            { x = x, y = y }

        flagship_ =
            flagship == Just pos

        gold_ =
            List.member pos rest

        silver_ =
            List.member pos silver

        changed =
            (List.member pos lastShips
                && not (List.member pos currentShips)
            )
                || ((not <| List.member pos lastShips)
                        && List.member pos currentShips
                   )

        changedPrev =
            (List.member pos beforeLastShips
                && not (List.member pos currentShips)
            )
                || ((not <| List.member pos beforeLastShips)
                        && List.member pos currentShips
                   )

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

             else if changed then
                rgb 0.5 0.4 0.2

             else if changedPrev then
                rgb 0.6 0.5 0.3

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
                (label x y)

         else
            label x y
        )


label x y =
    el [ Font.color (rgba 1 1 1 0.2) ]
        (text
            ((Array.get x
                (Array.fromList
                    [ "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K" ]
                )
                |> Maybe.withDefault ""
             )
                ++ String.fromInt (11 - y)
            )
        )

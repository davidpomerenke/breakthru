module Json exposing (..)

-- elm-package install -- yes noredink/elm-decode-pipeline

import Json.Decode as D
import Json.Decode.Pipeline exposing (..)
import Json.Encode as E
import Model exposing (Coordinate, Model, Player(..))


decodeCoordinate : D.Decoder Coordinate
decodeCoordinate =
    D.succeed Coordinate
        |> required "x" D.int
        |> required "y" D.int


toMaybePlayer : Maybe String -> D.Decoder (Maybe Player)
toMaybePlayer p =
    if p == Just "Gold" then
        D.succeed (Just Gold)

    else if p == Just "Silver" then
        D.succeed (Just Silver)

    else
        D.fail "Invalid player name."


toPlayer : String -> D.Decoder Player
toPlayer p =
    if p == "Gold" then
        D.succeed Gold

    else if p == "Silver" then
        D.succeed Silver

    else
        D.fail "Invalid player name."


decodeGold : D.Decoder ( Maybe Coordinate, List Coordinate )
decodeGold =
    D.map2 Tuple.pair
        (D.index 0 (D.nullable decodeCoordinate))
        (D.index 1 (D.list decodeCoordinate))


decodeModel : D.Decoder Model
decodeModel =
    D.succeed Model
        |> required "lastPlayer" (D.nullable D.string |> D.andThen toMaybePlayer)
        |> required "player" (D.string |> D.andThen toPlayer)
        |> required "gold" decodeGold
        |> required "silver" (D.list decodeCoordinate)


encodeCoordinate : Coordinate -> E.Value
encodeCoordinate { x, y } =
    E.object
        [ ( "x", E.int x )
        , ( "y", E.int y )
        ]


encodePlayer : Maybe Player -> E.Value
encodePlayer player =
    case player of
        Just Gold ->
            E.string "Gold"

        Just Silver ->
            E.string "Silver"

        Nothing ->
            E.null


encodeFlagship : Maybe Coordinate -> E.Value
encodeFlagship flagship =
    case flagship of
        Just coordinate ->
            encodeCoordinate coordinate

        Nothing ->
            E.null


encodeMove : Model -> E.Value
encodeMove { lastPlayer, player, gold, silver } =
    let
        ( flagship, rest ) =
            gold
    in
    E.object
        [ ( "lastPlayer", encodePlayer lastPlayer )
        , ( "player", encodePlayer (Just player) )
        , ( "gold", E.list identity [ encodeFlagship flagship, E.list encodeCoordinate rest ] )
        , ( "silver", E.list encodeCoordinate silver )
        ]

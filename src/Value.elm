module Value exposing (..)

import Dict exposing (Dict)


type Entry a
    = Single String (Value a)
    | Concurrent (Dict String (Value a))


type Value a
    = Value a
    | Tomb (TombValue a)


type TombValue a
    = TombValue a
    | TombUnknown


type Operation a
    = Insert a
    | Remove


mapEntry : (a -> b) -> Entry a -> Entry b
mapEntry fun entry =
    let
        mapValue v =
            case v of
                Value v ->
                    fun v |> Value

                Tomb (TombValue v) ->
                    fun v |> TombValue |> Tomb

                Tomb TombUnknown ->
                    Tomb TombUnknown
    in
        case entry of
            Single o v ->
                Single o <|
                    mapValue v

            Concurrent dict ->
                Dict.map (\_ -> mapValue) dict
                    |> Concurrent

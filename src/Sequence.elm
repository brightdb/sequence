module Sequence exposing (..)

import Dict exposing (Dict)
import Value exposing (..)
import List.Extra
import Tuple


type alias Sequence a =
    Dict Path (Entry a)


type alias Op b =
    { origin : String
    , target : String
    , path : Path
    , op : Operation b
    }


apply : List (Op a) -> Sequence a -> Sequence a
apply ops seq =
    List.foldl
        (\{ path, target, op } seq ->
            case op of
                Insert v ->
                    insert path target v seq

                Remove ->
                    remove path target seq
        )
        seq
        ops


insert : Path -> String -> a -> Sequence a -> Sequence a
insert path origin item seq =
    case Dict.get path seq of
        Nothing ->
            Dict.insert path (Single origin (Value item)) seq

        Just (Single origin2 value) ->
            if origin /= origin2 then
                Dict.insert path
                    (Dict.insert origin (Value item) Dict.empty
                        |> Dict.insert origin2 value
                        |> Concurrent
                    )
                    seq
            else if value == Tomb TombUnknown then
                Dict.insert path
                    (Single origin2 (Tomb (TombValue item)))
                    seq
            else
                seq

        Just (Concurrent dict) ->
            case Dict.get origin dict of
                Just (Tomb TombUnknown) ->
                    Dict.insert origin (Tomb (TombValue item)) dict
                        |> Concurrent
                        |> (\c -> Dict.insert path c seq)

                Nothing ->
                    Dict.insert origin (Value item) dict
                        |> Concurrent
                        |> (\c -> Dict.insert path c seq)

                _ ->
                    seq


remove : Path -> String -> Sequence a -> Sequence a
remove path origin seq =
    case Dict.get path seq of
        Nothing ->
            Dict.insert path (Single origin (Tomb TombUnknown)) seq

        Just (Single origin2 value) ->
            if origin2 == origin then
                case value of
                    Value v ->
                        Dict.insert path (Single origin (Tomb (TombValue v))) seq

                    Tomb _ ->
                        seq
            else
                Dict.insert path
                    (Dict.insert origin (Tomb TombUnknown) Dict.empty
                        |> Dict.insert origin2 value
                        |> Concurrent
                    )
                    seq

        Just (Concurrent dict) ->
            case Dict.get origin dict of
                Just (Value v) ->
                    Dict.insert origin (Tomb (TombValue v)) dict
                        |> Concurrent
                        |> (\c -> Dict.insert path c seq)

                _ ->
                    seq


find : (a -> Bool) -> Sequence a -> Maybe a
find predicate seq =
    let
        foldValue value =
            case value of
                Value a ->
                    if predicate a then
                        Just a
                    else
                        Nothing

                Tomb _ ->
                    Nothing
    in
        Dict.foldl
            (\path entry result ->
                case result of
                    Just r ->
                        Just r

                    Nothing ->
                        case entry of
                            Single _ value ->
                                foldValue value

                            Concurrent dict ->
                                Dict.foldl
                                    (\origin value result ->
                                        case result of
                                            Just r ->
                                                Just r

                                            Nothing ->
                                                foldValue value
                                    )
                                    result
                                    dict
            )
            Nothing
            seq


toList : Bool -> Sequence a -> List a
toList withTombs =
    Dict.values >> List.map (entryToList withTombs) >> List.concat


entryToList : Bool -> Entry a -> List a
entryToList withTombs entry =
    case entry of
        Single origin (Value a) ->
            [ a ]

        Single origin (Tomb (TombValue a)) ->
            if withTombs then
                [ a ]
            else
                []

        Single origin (Tomb TombUnknown) ->
            []

        Concurrent dict ->
            Dict.values dict
                |> List.filterMap
                    (\a ->
                        case a of
                            Value a ->
                                Just a

                            Tomb (TombValue a) ->
                                if withTombs then
                                    Just a
                                else
                                    Nothing

                            Tomb TombUnknown ->
                                Nothing
                    )


replaceIf : (a -> Bool) -> a -> Sequence a -> Sequence a
replaceIf predicate item seq =
    let
        replaceIfValue value =
            case value of
                Value a ->
                    if predicate a then
                        Value item
                    else
                        Value a

                Tomb t ->
                    Tomb t
    in
        Dict.map
            (\path entry ->
                case entry of
                    Single origin value ->
                        replaceIfValue value
                            |> Single origin

                    Concurrent dict ->
                        Dict.map
                            (\origin value ->
                                replaceIfValue value
                            )
                            dict
                            |> Concurrent
            )
            seq


getNeighbors : Int -> Sequence a -> ( Maybe Path, Maybe Path )
getNeighbors int aSequence =
    let
        keys =
            Dict.keys aSequence
    in
        if int == List.length keys then
            ( List.Extra.last keys
            , Nothing
            )
        else if int == 0 then
            ( Nothing
            , List.head keys
            )
        else
            case List.drop (int - 1) keys |> List.take 2 of
                first :: second :: _ ->
                    ( Just first, Just second )

                first :: _ ->
                    ( Just first, Nothing )

                _ ->
                    ( Nothing, Nothing )


fromList : String -> List ( Path, a ) -> Sequence a
fromList target list =
    Dict.fromList list
        |> Dict.map
            (\path item ->
                Single target (Value item)
            )


first : Sequence a -> Maybe ( Path, Entry a )
first seq =
    Dict.toList seq
        |> List.head


empty : Sequence a
empty =
    Dict.empty


contains : a -> Sequence a -> Bool
contains item seq =
    Dict.values seq
        |> List.Extra.find
            (\entry ->
                case entry of
                    Single _ (Value i) ->
                        i == item

                    Concurrent dict ->
                        Dict.values dict
                            |> List.Extra.find ((==) (Value item))
                            |> (/=) Nothing

                    _ ->
                        False
            )
        |> (/=) Nothing


map : (a -> b) -> Sequence a -> Sequence b
map fun =
    Dict.map
        (\_ entry ->
            mapEntry fun entry
        )


foldl : (Path -> Entry a -> b -> b) -> b -> Sequence a -> b
foldl =
    Dict.foldl


get : Path -> String -> Sequence a -> Maybe (Value a)
get path target seq =
    Dict.get path seq
        |> Maybe.andThen
            (\entry ->
                case entry of
                    Single t v ->
                        if t == target then
                            Just v
                        else
                            Nothing

                    Concurrent mvr ->
                        Dict.get target mvr
            )


union : Sequence a -> Sequence a -> Sequence a
union =
    Dict.union

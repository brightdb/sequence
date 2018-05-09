module Sequence exposing (..)

import IntDict exposing (IntDict)
import Dict exposing (Dict)
import Value exposing (..)
import List.Extra
import Tuple
import Random.Pcg as Rand
import Bitwise exposing (..)


type alias Sequence a =
    IntDict (Entry a)


type alias Path =
    Int


type alias Op b =
    { origin : String
    , target : String
    , path : Path
    , op : Operation b
    }


offset =
    4


maxLayer =
    4


maxBit =
    31


gauss x =
    (x ^ 2 + x)
        // 2


bits l =
    (gauss (offset + l))
        - (gauss (offset - 1))
        + 1


shift l =
    maxBit
        - bits l


layerMaskComplete l =
    (shiftLeftBy (bits l) 1 - 1)
        |> shiftLeftBy (shift l)


layerMask l =
    let
        bits =
            if l == 0 then
                offset + 1
            else
                offset + l
    in
        (shiftLeftBy bits 1 - 1)
            |> shiftLeftBy (shift l)


boundarySize l =
    4


layerFromRange : Path -> Path -> Int -> Int
layerFromRange start end l =
    let
        mask =
            layerMask l
    in
        if and start mask == and end mask && l < maxLayer then
            layerFromRange start end (l + 1)
        else
            l


layerFromPath : Path -> Int -> Int
layerFromPath path l =
    let
        mask =
            layerMask l
    in
        if and path mask == 0 && l > 0 then
            layerFromPath path (l - 1)
        else
            l


layerSize l =
    2 ^ (offset + l)


alloc : Path -> Path -> Path
alloc start end =
    let
        start_ =
            min start end

        end_ =
            max start end

        layer =
            layerFromPath start_ maxLayer

        layerEnd =
            layerFromPath end_ maxLayer

        --posStartOnLayer
        p =
            layerMask layer
                |> and start_
                |> shiftRightBy (shift layer)

        --posEndOnLayer
        q =
            if layer > layerEnd then
                layerSize layer
            else
                layerMask layer
                    |> and end_
                    |> shiftRightBy (shift layer)

        seed2 =
            start + end |> Rand.initialSeed

        ( layer_, p_, q_ ) =
            if q - p > 1 then
                ( layer, p, q )
            else if layer < maxLayer then
                ( layer + 1, 0, layerSize (layer + 1) )
            else
                ( layer, p, q )

        seed =
            layerMaskComplete layer_ |> Rand.initialSeed

        ( b, _ ) =
            Rand.step Rand.bool seed

        boundary =
            boundarySize layer_

        ( lower, upper ) =
            if True then
                ( p_ + 1, p_ + boundary |> min (q_ - 1) )
            else
                ( q_ - boundary |> max (p_ + 1), q_ - 1 )

        lm =
            if layer_ > 0 then
                layer_
                    - 1
                    |> layerMaskComplete
            else
                0

        offsetStart =
            and start lm
    in
        Rand.step (Rand.int lower upper) seed2
            |> Tuple.first
            |> shiftLeftBy (shift layer_)
            |> (+) offsetStart


createInsert : String -> Path -> a -> Op a
createInsert target path a =
    Insert a
        |> Op target target path


createRemove : String -> String -> Path -> Op a
createRemove origin target path =
    Remove
        |> Op origin target path


apply : List (Op a) -> Sequence a -> ( Sequence a, List (Op a) )
apply ops seq =
    List.foldr
        (\op ( seq, newOps ) ->
            let
                ( seq_, success ) =
                    case op.op of
                        Insert v ->
                            insert op.path op.target v seq

                        Remove ->
                            remove op.path op.target seq
            in
                ( seq_
                , if success then
                    op :: newOps
                  else
                    newOps
                )
        )
        ( seq, [] )
        ops


insert : Path -> String -> a -> Sequence a -> ( Sequence a, Bool )
insert path origin item seq =
    case IntDict.get path seq of
        Nothing ->
            ( IntDict.insert path (Single origin (Value item)) seq
            , True
            )

        Just (Single origin2 value) ->
            if origin /= origin2 then
                ( IntDict.insert path
                    (Dict.insert origin (Value item) Dict.empty
                        |> Dict.insert origin2 value
                        |> Concurrent
                    )
                    seq
                , True
                )
            else if value == Tomb TombUnknown then
                ( IntDict.insert path
                    (Single origin2 (Tomb (TombValue item)))
                    seq
                , True
                )
            else
                ( seq, False )

        Just (Concurrent dict) ->
            case Dict.get origin dict of
                Just (Tomb TombUnknown) ->
                    ( Dict.insert origin (Tomb (TombValue item)) dict
                        |> Concurrent
                        |> (\c -> IntDict.insert path c seq)
                    , True
                    )

                Nothing ->
                    ( Dict.insert origin (Value item) dict
                        |> Concurrent
                        |> (\c -> IntDict.insert path c seq)
                    , True
                    )

                _ ->
                    ( seq, False )


remove : Path -> String -> Sequence a -> ( Sequence a, Bool )
remove path origin seq =
    case IntDict.get path seq of
        Nothing ->
            ( IntDict.insert path (Single origin (Tomb TombUnknown)) seq
            , True
            )

        Just (Single origin2 value) ->
            if origin2 == origin then
                case value of
                    Value v ->
                        ( IntDict.insert path (Single origin (Tomb (TombValue v))) seq
                        , True
                        )

                    Tomb _ ->
                        ( seq, False )
            else
                ( IntDict.insert path
                    (Dict.insert origin (Tomb TombUnknown) Dict.empty
                        |> Dict.insert origin2 value
                        |> Concurrent
                    )
                    seq
                , True
                )

        Just (Concurrent dict) ->
            case Dict.get origin dict of
                Just (Value v) ->
                    ( Dict.insert origin (Tomb (TombValue v)) dict
                        |> Concurrent
                        |> (\c -> IntDict.insert path c seq)
                    , True
                    )

                _ ->
                    ( seq, False )


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
        IntDict.foldl
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
    IntDict.values >> List.map (entryToList withTombs) >> List.concat


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
        IntDict.map
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


fromList : String -> List ( Path, a ) -> Sequence a
fromList target list =
    IntDict.fromList list
        |> IntDict.map
            (\path item ->
                Single target (Value item)
            )


first : Sequence a -> Maybe ( Path, Entry a )
first =
    IntDict.findMin


last : Sequence a -> Maybe ( Path, Entry a )
last =
    IntDict.findMax


before : Path -> Sequence a -> Maybe ( Path, Entry a )
before =
    IntDict.before


after : Path -> Sequence a -> Maybe ( Path, Entry a )
after =
    IntDict.after


empty : Sequence a
empty =
    IntDict.empty


contains : a -> Sequence a -> Bool
contains item seq =
    IntDict.values seq
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
    IntDict.map
        (\_ entry ->
            mapEntry fun entry
        )


foldl : (Path -> Entry a -> b -> b) -> b -> Sequence a -> b
foldl =
    IntDict.foldl


foldr : (Path -> Entry a -> b -> b) -> b -> Sequence a -> b
foldr =
    IntDict.foldr


get : Path -> String -> Sequence a -> Maybe (Value a)
get path target seq =
    IntDict.get path seq
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
    IntDict.union

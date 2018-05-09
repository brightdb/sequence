module Sequence
    exposing
        ( Sequence
        , Path
        , Op
        , Entry(..)
        , Value(..)
        , TombValue(..)
        , Operation(..)
        , MVR
        , alloc
        , createInsert
        , createRemove
        , apply
        , empty
        , first
        , last
        , after
        , before
        , find
        )

import IntDict exposing (IntDict)
import Dict exposing (Dict)
import List.Extra
import Tuple exposing (..)
import Random.Pcg as Rand
import Bitwise exposing (..)


type Sequence a
    = Sequence (IntDict (Entry a))


type Path
    = Path Int


type alias Op b =
    { origin : String
    , target : String
    , path : Path
    , op : Operation b
    }


type Entry a
    = Single String (Value a)
    | Concurrent (MVR a)


type Value a
    = Value a
    | Tomb (TombValue a)


type TombValue a
    = TombValue a
    | TombUnknown


type Operation a
    = Insert a
    | Remove


type MVR a
    = MVR (Dict String (Value a))


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

            Concurrent (MVR dict) ->
                Dict.map (\_ -> mapValue) dict
                    |> MVR
                    |> Concurrent


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


layerFromRange : Int -> Int -> Int -> Int
layerFromRange start end l =
    let
        mask =
            layerMask l
    in
        if and start mask == and end mask && l < maxLayer then
            layerFromRange start end (l + 1)
        else
            l


layerFromPath : Int -> Int -> Int
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
alloc (Path start) (Path end) =
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
            |> Path


createInsert : String -> Path -> a -> Op a
createInsert target path a =
    Insert a
        |> Op target target path


createRemove : String -> String -> Path -> Op a
createRemove origin target path =
    Remove
        |> Op origin target path


apply : List (Op a) -> Sequence a -> ( Sequence a, List (Op a) )
apply ops (Sequence seq) =
    List.foldr
        (\op ( seq, newOps ) ->
            let
                path =
                    case op.path of
                        Path p ->
                            p

                ( seq_, success ) =
                    case op.op of
                        Insert v ->
                            insert path op.target v seq

                        Remove ->
                            remove path op.target seq
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
        |> mapFirst Sequence


insert : Int -> String -> a -> IntDict (Entry a) -> ( IntDict (Entry a), Bool )
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
                        |> MVR
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

        Just (Concurrent (MVR dict)) ->
            case Dict.get origin dict of
                Just (Tomb TombUnknown) ->
                    ( Dict.insert origin (Tomb (TombValue item)) dict
                        |> MVR
                        |> Concurrent
                        |> (\c -> IntDict.insert path c seq)
                    , True
                    )

                Nothing ->
                    ( Dict.insert origin (Value item) dict
                        |> MVR
                        |> Concurrent
                        |> (\c -> IntDict.insert path c seq)
                    , True
                    )

                _ ->
                    ( seq, False )


remove : Int -> String -> IntDict (Entry a) -> ( IntDict (Entry a), Bool )
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
                        |> MVR
                        |> Concurrent
                    )
                    seq
                , True
                )

        Just (Concurrent (MVR dict)) ->
            case Dict.get origin dict of
                Just (Value v) ->
                    ( Dict.insert origin (Tomb (TombValue v)) dict
                        |> MVR
                        |> Concurrent
                        |> (\c -> IntDict.insert path c seq)
                    , True
                    )

                _ ->
                    ( seq, False )


find : (a -> Bool) -> Sequence a -> Maybe a
find predicate (Sequence seq) =
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

                            Concurrent (MVR dict) ->
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


values : Sequence a -> List (Entry a)
values (Sequence seq) =
    IntDict.values seq


first : Sequence a -> Maybe ( Path, Entry a )
first (Sequence seq) =
    IntDict.findMin seq
        |> Maybe.map (mapFirst Path)


last : Sequence a -> Maybe ( Path, Entry a )
last (Sequence seq) =
    IntDict.findMax seq
        |> Maybe.map (mapFirst Path)


before : Path -> Sequence a -> Maybe ( Path, Entry a )
before (Path path) (Sequence seq) =
    IntDict.before path seq
        |> Maybe.map (mapFirst Path)


after : Path -> Sequence a -> Maybe ( Path, Entry a )
after (Path path) (Sequence seq) =
    IntDict.after path seq
        |> Maybe.map (mapFirst Path)


empty : Sequence a
empty =
    Sequence IntDict.empty


map : (a -> b) -> Sequence a -> Sequence b
map fun (Sequence seq) =
    IntDict.map
        (\_ entry ->
            mapEntry fun entry
        )
        seq
        |> Sequence


foldl : (Path -> Entry a -> b -> b) -> b -> Sequence a -> b
foldl fun init (Sequence seq) =
    IntDict.foldl (\int -> fun (Path int)) init seq


foldr : (Path -> Entry a -> b -> b) -> b -> Sequence a -> b
foldr fun init (Sequence seq) =
    IntDict.foldr (\int -> fun (Path int)) init seq


get : Path -> String -> Sequence a -> Maybe (Value a)
get (Path path) target (Sequence seq) =
    IntDict.get path seq
        |> Maybe.andThen
            (\entry ->
                case entry of
                    Single t v ->
                        if t == target then
                            Just v
                        else
                            Nothing

                    Concurrent (MVR mvr) ->
                        Dict.get target mvr
            )

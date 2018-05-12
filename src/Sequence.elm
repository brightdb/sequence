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
        , get
        , first
        , last
        , after
        , before
        , foldl
        , foldr
        , mvrToList
        , decodeOp
        , encodeOp
        , minPath
        , maxPath
        )

{-| This is a prototype of a CRDT for sequential data written in Elm.

**Work in progress!** Use with caution.

Implementation stems from Nedelec et al. "LSEQ: an adaptive structure for sequences in distributed collaborative editing" (2013).


# Definition

@docs Sequence, Path, Op, Entry, Value, TombValue, Operation, MVR


# Operations

@docs alloc, createInsert, createRemove, apply


# Sequence handling

@docs empty, get, first, last, after, before, foldl, foldr


# MVR handling

@docs mvrToList


# Decoders

@docs decodeOp, encodeOp


# Constants

@docs minPath, maxPath

-}

import IntDict exposing (IntDict)
import Dict exposing (Dict)
import List.Extra
import Tuple exposing (..)
import Random.Pcg as Rand
import Bitwise exposing (..)
import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc


{-| The data type itself. Takes a user-defined type as its value type.
-}
type Sequence a
    = Sequence (IntDict (Entry a))


{-| The unique identifier of a Entry's position in the sequence.
It's called "path" because the data type consists of multiple layers
comparable to a filesystem's directories. A path to a value works like
a path to a file.
-}
type Path
    = Path Int


{-| All data manipulation happens through `Operation`s. It either is an `Insert a` or `Remove`.
-}
type Operation a
    = Insert a
    | Remove


{-| An entry in the sequence might be a single value (just one user applied an
operation on it) or a multi-value registry (MVR) in case multiple users a
applied operation at this path.
-}
type Entry a
    = Single String (Value a)
    | Concurrent (MVR a)


{-| A multi-value registry is a dictionary of user identifiers and Values.
Get its contents with [`mvrToList`](#mvrToList).
-}
type MVR a
    = MVR (Dict String (Value a))


{-| The actual value. After applying an Insert operation it is `Value a`, after
a Remove operation it is a `Tomb (TombValue a)`. So storage of removed values never gets
freed.
-}
type Value a
    = Value a
    | Tomb (TombValue a)


{-| If a remove operation is applied at a path which has a value, it's turned
into `Tomb (TombValue a)`. If the operation is applied on a free path it
becomes `TombUnknown`. If an insert operation is applied after this, it's
turned into `TombValue a` finally. This way it does not matter in which order
insert and remove operation are applied.
-}
type TombValue a
    = TombValue a
    | TombUnknown


{-| The complete self-contained op(eration). `origin` is the identifier for the
creating user/instance. If the Entry at `path` already contains a MVR apply the
operation at
`target`'s value.
-}
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


{-| Allocate a path given it's lower and upper bounds (non-inclusive).
The bounds should be paths that are already taken and the possible path's
between should all be free.
-}
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
            |> Debug.log "alloc"
            |> Path


{-| Create an insert operation. Pass it the user identifier, a path and the
value to insert.
-}
createInsert : String -> Path -> a -> Op a
createInsert target path a =
    Insert a
        |> Op target target path


{-| Create a remove operation. Pass it the removing user's identifier, the user
identifier of the removed value (ie. to target it in a MVR) and the path.
-}
createRemove : String -> String -> Path -> Op a
createRemove origin target path =
    Remove
        |> Op origin target path


{-| Apply multiple ops at once to a sequence. Returns the updated sequence and
list of successful operations (which actually changed something).
-}
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


{-| Return the first entry of the sequence and its path.
If the sequence is empty returns `Nothing`.
-}
first : Sequence a -> Maybe ( Path, Entry a )
first (Sequence seq) =
    IntDict.findMin seq
        |> Maybe.map (mapFirst Path)


{-| Return the last entry of the sequence and its path.
If the sequence is empty returns `Nothing`.
-}
last : Sequence a -> Maybe ( Path, Entry a )
last (Sequence seq) =
    IntDict.findMax seq
        |> Maybe.map (mapFirst Path)


{-| Return the entry and its path before the given path in the sequence.
Returns `Nothing` if there none.
-}
before : Path -> Sequence a -> Maybe ( Path, Entry a )
before (Path path) (Sequence seq) =
    IntDict.before path seq
        |> Maybe.map (mapFirst Path)


{-| Return the entry and its path after the given path in the sequence.
Returns `Nothing` if there none.
-}
after : Path -> Sequence a -> Maybe ( Path, Entry a )
after (Path path) (Sequence seq) =
    IntDict.after path seq
        |> Maybe.map (mapFirst Path)


{-| An empty sequence.
-}
empty : Sequence a
empty =
    Sequence IntDict.empty


{-| Fold a sequence from the left.
-}
foldl : (Path -> Entry a -> b -> b) -> b -> Sequence a -> b
foldl fun init (Sequence seq) =
    IntDict.foldl (\int -> fun (Path int)) init seq


{-| Fold a sequence from the right.
-}
foldr : (Path -> Entry a -> b -> b) -> b -> Sequence a -> b
foldr fun init (Sequence seq) =
    IntDict.foldr (\int -> fun (Path int)) init seq


{-| Lookup an entry at the given path.
-}
get : Path -> Sequence a -> Maybe (Entry a)
get (Path path) (Sequence seq) =
    IntDict.get path seq


{-| Inspect an MVR by turning it into a list of tuples of the user identifier and the `Value`.
-}
mvrToList : MVR a -> List ( String, Value a )
mvrToList (MVR mvr) =
    Dict.toList mvr


{-| Decode an Op.
-}
decodeOp : Decoder a -> Decoder (Op a)
decodeOp decoder =
    Dec.map4 Op
        (Dec.index 0 Dec.string)
        (Dec.index 1 Dec.string)
        (Dec.index 2 decodePath)
        (Dec.index 3 <| decodeOperation decoder)


decodePath =
    Dec.map Path Dec.int


decodeOperation : Decoder a -> Decoder (Operation a)
decodeOperation decoder =
    Dec.oneOf
        [ Dec.field "i" decoder
            |> Dec.map Insert
        , Dec.string
            |> Dec.andThen
                (\str ->
                    if str == "r" then
                        Dec.succeed Remove
                    else
                        Dec.fail "unknown operation"
                )
        ]


{-| Encoder an Op given an value specific encoder.
-}
encodeOp : (a -> Enc.Value) -> Op a -> Enc.Value
encodeOp encoder op =
    [ Enc.string op.origin
    , Enc.string op.target
    , encodePath op.path
    , encodeOperation encoder op.op
    ]
        |> Enc.list


encodePath (Path path) =
    Enc.int path


encodeOperation : (a -> Enc.Value) -> Operation a -> Enc.Value
encodeOperation encoder op =
    case op of
        Insert a ->
            [ ( "i"
              , encoder a
              )
            ]
                |> Enc.object

        Remove ->
            Enc.string "r"


{-| The greatest path possible
-}
maxPath : Path
maxPath =
    shiftLeftBy 30 1
        |> Path


{-| The lowest path possible
-}
minPath : Path
minPath =
    Path 0

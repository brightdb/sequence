module Sequence exposing
    ( Sequence, Path, Op, Entry(..), Value(..), TombValue(..), Operation(..), MVR
    , alloc, createInsert, createRemove, apply
    , empty, get, first, last, after, before, foldl, foldr
    , mvrToList
    , decodeOp, encodeOp
    , minPath, maxPath, path, comparePath, pathToString
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


# Paths

@docs minPath, maxPath, path, comparePath, pathToString

-}

import Bitwise exposing (..)
import Dict exposing (Dict)
import IntDict as Layer exposing (IntDict)
import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc
import List.Extra
import Random as Rand
import Tuple exposing (..)


{-| The data type itself. Takes a user-defined type as its value type.
-}
type Sequence a
    = Sequence (Layer a)


type alias Layer a =
    IntDict (LayerItem a)


type LayerItem a
    = Item (Entry a)
    | SubLayer (Layer a)


{-| The unique identifier of a Entry's position in the sequence.
It consists of a list of positions, one for each layer in the data structure.

Eg. given this data structure:

    [a|b|c|d]
      |
      [x|y]

  - 'a' has path `[0]`
  - 'c' has path `[2]`
  - 'x' has path `[1,0]`

-}
type Path
    = Path ( Int, List Int )


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


emptyLayer =
    Layer.empty


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
    gauss (offset + l)
        - gauss (offset - 1)
        + 1


shift l =
    maxBit
        - bits l


layerMaskComplete l =
    (shiftLeftBy (bits l) 1 - 1)
        |> shiftLeftBy (shift l)


layerMask l =
    let
        bits_ =
            if l == 0 then
                offset + 1

            else
                offset + l
    in
    (shiftLeftBy bits_ 1 - 1)
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
layerFromPath path_ l =
    let
        mask =
            layerMask l
    in
    if and path_ mask == 0 && l > 0 then
        layerFromPath path_ (l - 1)

    else
        l


layerSize l =
    2 ^ (offset + l)


padList len list =
    list ++ List.repeat (len - List.length list) 0


splitTail i list =
    List.reverse (i :: list)
        |> List.Extra.uncons
        |> Maybe.map (mapSecond List.reverse)
        |> Maybe.withDefault ( i, [] )


getCommonPrefix ( sHead, sTail ) ( eHead, eTail ) =
    let
        maxLen =
            max (List.length sTail) (List.length eTail)

        ( p, prefixP ) =
            padList maxLen sTail
                |> splitTail sHead

        ( q_, prefixQ ) =
            padList maxLen eTail
                |> splitTail eHead

        q =
            if prefixP == prefixQ then
                q_

            else
                List.length prefixP |> layerSize
    in
    if abs (p - q) > 1 then
        ( prefixP, p, q )

    else
        ( prefixP ++ [ p ], 0, List.length prefixP + 1 |> layerSize )


{-| Allocate a path given it's lower and upper bounds (non-inclusive).
The bounds should be paths that are already taken and the possible path's
between should all be free.
-}
alloc : Path -> Path -> Path
alloc (Path ( sHead, sTail )) (Path ( eHead, eTail )) =
    let
        ( prefix, p, q ) =
            getCommonPrefix ( sHead, sTail ) ( eHead, eTail )

        seed =
            List.sum prefix
                + p
                + q
                |> Rand.initialSeed

        seed2 =
            List.sum prefix |> Rand.initialSeed

        ( b, _ ) =
            Rand.step (Rand.uniform True [ False ]) seed2

        boundary =
            List.length prefix
                |> boundarySize

        ( lower, upper ) =
            if b then
                ( p + 1, p + boundary |> min (q - 1) )

            else
                ( q - boundary |> max (p + 1), q - 1 )

        i =
            Rand.step (Rand.int lower upper) seed
                |> Tuple.first
    in
    case prefix of
        [] ->
            Path ( i, [] )

        head :: tail ->
            tail
                ++ [ i ]
                |> (\b_ -> ( head, b_ ))
                |> Path


{-| Create an insert operation. Pass it the user identifier, a path and the
value to insert.
-}
createInsert : String -> Path -> a -> Op a
createInsert target path_ a =
    Insert a
        |> Op target target path_


{-| Create a remove operation. Pass it the removing user's identifier, the user
identifier of the removed value (ie. to target it in a MVR) and the path.
-}
createRemove : String -> String -> Path -> Op a
createRemove origin target path_ =
    Remove
        |> Op origin target path_


{-| Apply multiple ops at once to a sequence. Returns the updated sequence and
list of successful operations (which actually changed something).
-}
apply : List (Op a) -> Sequence a -> ( Sequence a, List (Op a) )
apply ops (Sequence seq) =
    List.foldl
        (\op ( seq2, newOps ) ->
            let
                ( seq3, success ) =
                    case op.op of
                        Insert v ->
                            insert op.path op.target v seq2

                        Remove ->
                            remove op.path op.target seq2
            in
            ( seq3
            , if success then
                newOps ++ [ op ]

              else
                newOps
            )
        )
        ( seq, [] )
        ops
        |> mapFirst Sequence


insert : Path -> String -> a -> Layer a -> ( Layer a, Bool )
insert (Path ( head, tail )) origin value layer =
    case tail of
        [] ->
            case Layer.get head layer of
                Nothing ->
                    ( Layer.insert head (Item (Single origin (Value value))) layer
                    , True
                    )

                Just (Item entry) ->
                    insertAtEntry origin value entry
                        |> mapFirst (\entry_ -> Layer.insert head (Item entry_) layer)

                Just (SubLayer subLayer) ->
                    insert (Path ( 0, [] )) origin value subLayer
                        |> mapFirst
                            (\newLayer ->
                                Layer.insert head (SubLayer newLayer) layer
                            )

        next :: rest ->
            case Layer.get head layer of
                Nothing ->
                    insert (Path ( next, rest )) origin value emptyLayer
                        |> mapFirst
                            (\newLayer ->
                                Layer.insert head (SubLayer newLayer) layer
                            )

                Just (Item b) ->
                    Layer.insert 0 (Item b) emptyLayer
                        |> insert (Path ( next, rest )) origin value
                        |> mapFirst
                            (\newLayer ->
                                Layer.insert head (SubLayer newLayer) layer
                            )

                Just (SubLayer subLayer) ->
                    insert (Path ( next, rest )) origin value subLayer
                        |> mapFirst
                            (\newLayer ->
                                Layer.insert head (SubLayer newLayer) layer
                            )


insertAtEntry : String -> a -> Entry a -> ( Entry a, Bool )
insertAtEntry origin value entry =
    case entry of
        Single origin2 v ->
            if origin /= origin2 then
                ( Dict.insert origin (Value value) Dict.empty
                    |> Dict.insert origin2 v
                    |> MVR
                    |> Concurrent
                , True
                )

            else if v == Tomb TombUnknown then
                ( Single origin2 (Tomb (TombValue value))
                , True
                )

            else
                ( entry, False )

        Concurrent (MVR dict) ->
            case Dict.get origin dict of
                Just (Tomb TombUnknown) ->
                    ( Dict.insert origin (Tomb (TombValue value)) dict
                        |> MVR
                        |> Concurrent
                    , True
                    )

                Nothing ->
                    ( Dict.insert origin (Value value) dict
                        |> MVR
                        |> Concurrent
                    , True
                    )

                _ ->
                    ( entry, False )


remove : Path -> String -> Layer a -> ( Layer a, Bool )
remove (Path ( head, tail )) origin layer =
    case tail of
        [] ->
            case Layer.get head layer of
                Nothing ->
                    ( layer, False )

                Just (Item entry) ->
                    removeAtEntry origin entry
                        |> mapFirst (\entry_ -> Layer.insert head (Item entry_) layer)

                Just (SubLayer subLayer) ->
                    remove (Path ( 0, [] )) origin subLayer
                        |> mapFirst
                            (\newLayer ->
                                Layer.insert head (SubLayer newLayer) layer
                            )

        next :: rest ->
            case Layer.get head layer of
                Nothing ->
                    ( layer, False )

                Just (Item a) ->
                    ( layer, False )

                Just (SubLayer subLayer) ->
                    remove (Path ( next, rest )) origin subLayer
                        |> mapFirst
                            (\newLayer ->
                                Layer.insert head (SubLayer newLayer) layer
                            )


removeAtEntry : String -> Entry a -> ( Entry a, Bool )
removeAtEntry origin entry =
    case entry of
        Single origin2 value ->
            if origin2 == origin then
                case value of
                    Value v ->
                        ( Single origin (Tomb (TombValue v))
                        , True
                        )

                    Tomb _ ->
                        ( entry, False )

            else
                ( Dict.insert origin (Tomb TombUnknown) Dict.empty
                    |> Dict.insert origin2 value
                    |> MVR
                    |> Concurrent
                , True
                )

        Concurrent (MVR dict) ->
            case Dict.get origin dict of
                Just (Value v) ->
                    ( Dict.insert origin (Tomb (TombValue v)) dict
                        |> MVR
                        |> Concurrent
                    , True
                    )

                _ ->
                    ( entry, False )


find : (a -> Bool) -> Sequence a -> Maybe a
find predicate (Sequence seq) =
    findInLayer predicate seq


findInLayer : (a -> Bool) -> Layer a -> Maybe a
findInLayer predicate layer =
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
    Layer.foldl
        (\path_ item result ->
            case result of
                Just r ->
                    Just r

                Nothing ->
                    case item of
                        SubLayer subLayer ->
                            findInLayer predicate subLayer

                        Item entry ->
                            case entry of
                                Single _ value ->
                                    foldValue value

                                Concurrent (MVR dict) ->
                                    Dict.foldl
                                        (\origin value result_ ->
                                            case result_ of
                                                Just r ->
                                                    Just r

                                                Nothing ->
                                                    foldValue value
                                        )
                                        result
                                        dict
        )
        Nothing
        layer


values : Sequence a -> List (Entry a)
values (Sequence seq) =
    valuesInLayer seq


valuesInLayer : Layer a -> List (Entry a)
valuesInLayer layer =
    Layer.foldr
        (\path_ item values_ ->
            case item of
                SubLayer subLayer ->
                    valuesInLayer subLayer ++ values_

                Item entry ->
                    entry :: values_
        )
        []
        layer


{-| Return the first entry of the sequence and its path.
If the sequence is empty returns `Nothing`.
-}
first : Sequence a -> Maybe ( Path, Entry a )
first (Sequence seq) =
    firstInLayer seq
        |> Maybe.map (mapFirst Path)


firstInLayer layer =
    case Layer.findMin layer of
        Nothing ->
            Nothing

        Just ( p, SubLayer subLayer ) ->
            firstInLayer subLayer
                |> Maybe.map
                    (mapFirst
                        (\( head, tail ) ->
                            ( p, head :: tail )
                        )
                    )

        Just ( p, Item entry ) ->
            Just ( ( p, [] ), entry )


{-| Return the last entry of the sequence and its path.
If the sequence is empty returns `Nothing`.
-}
last : Sequence a -> Maybe ( Path, Entry a )
last (Sequence seq) =
    lastInLayer seq
        |> Maybe.map (mapFirst Path)


lastInLayer layer =
    case Layer.findMax layer of
        Nothing ->
            Nothing

        Just ( p, SubLayer subLayer ) ->
            lastInLayer subLayer
                |> Maybe.map
                    (mapFirst
                        (\( head, tail ) ->
                            ( p, head :: tail )
                        )
                    )

        Just ( p, Item entry ) ->
            Just ( ( p, [] ), entry )


{-| Return the entry and its path before the given path in the sequence.
Returns `Nothing` if there none.
-}
before : Path -> Sequence a -> Maybe ( Path, Entry a )
before (Path path_) (Sequence seq) =
    beforeInLayer path_ seq
        |> Maybe.map (mapFirst Path)


beforeInLayer ( head, tail ) layer =
    case tail of
        [] ->
            case Layer.before head layer of
                Nothing ->
                    Nothing

                Just ( p, Item entry ) ->
                    Just ( ( p, [] ), entry )

                Just ( p, SubLayer subLayer ) ->
                    case lastInLayer subLayer of
                        Nothing ->
                            Nothing

                        Just ( ( pHead, pTail ), entry ) ->
                            Just ( ( p, pHead :: pTail ), entry )

        next :: tail_ ->
            case Layer.get head layer of
                Nothing ->
                    Nothing

                Just (Item entry) ->
                    Nothing

                Just (SubLayer subLayer) ->
                    case beforeInLayer ( next, tail_ ) subLayer of
                        Nothing ->
                            case Layer.before head layer of
                                Nothing ->
                                    Nothing

                                Just ( i, Item entry ) ->
                                    Just ( ( i, [] ), entry )

                                Just ( i, SubLayer subLayer_ ) ->
                                    case lastInLayer subLayer_ of
                                        Nothing ->
                                            Nothing

                                        Just ( ( pHead, pTail ), entry ) ->
                                            Just ( ( i, pHead :: pTail ), entry )

                        Just ( ( pHead, pTail ), entry ) ->
                            Just ( ( head, pHead :: pTail ), entry )


{-| Return the entry and its path after the given path in the sequence.
Returns `Nothing` if there none.
-}
after : Path -> Sequence a -> Maybe ( Path, Entry a )
after (Path path_) (Sequence seq) =
    afterInLayer path_ seq
        |> Maybe.map (mapFirst Path)


afterInLayer ( head, tail ) layer =
    case tail of
        [] ->
            case Layer.after head layer of
                Nothing ->
                    Nothing

                Just ( p, Item entry ) ->
                    Just ( ( p, [] ), entry )

                Just ( p, SubLayer subLayer ) ->
                    case firstInLayer subLayer of
                        Nothing ->
                            Nothing

                        Just ( ( pHead, pTail ), entry ) ->
                            Just ( ( p, pHead :: pTail ), entry )

        next :: tail_ ->
            case Layer.get head layer of
                Nothing ->
                    Nothing

                Just (Item entry) ->
                    Nothing

                Just (SubLayer subLayer) ->
                    case afterInLayer ( next, tail_ ) subLayer of
                        Nothing ->
                            case Layer.after head layer of
                                Nothing ->
                                    Nothing

                                Just ( i, Item entry ) ->
                                    Just ( ( i, [] ), entry )

                                Just ( i, SubLayer subLayer_ ) ->
                                    case firstInLayer subLayer_ of
                                        Nothing ->
                                            Nothing

                                        Just ( ( pHead, pTail ), entry ) ->
                                            Just ( ( i, pHead :: pTail ), entry )

                        Just ( ( pHead, pTail ), entry ) ->
                            Just ( ( head, pHead :: pTail ), entry )


{-| An empty sequence.
-}
empty : Sequence a
empty =
    Sequence Layer.empty


{-| Fold a sequence from the left.
-}
foldl : (Path -> Entry a -> b -> b) -> b -> Sequence a -> b
foldl fun init (Sequence seq) =
    foldlLayer [] fun init seq


foldlLayer prefix fun init layer =
    Layer.foldl
        (\i item result ->
            case item of
                Item entry ->
                    let
                        p =
                            case prefix of
                                [] ->
                                    ( i, [] )

                                head :: rest ->
                                    ( head, rest ++ [ i ] )
                    in
                    fun (Path p) entry result

                SubLayer subLayer ->
                    foldlLayer (prefix ++ [ i ]) fun result subLayer
        )
        init
        layer


{-| Fold a sequence from the right.
-}
foldr : (Path -> Entry a -> b -> b) -> b -> Sequence a -> b
foldr fun init (Sequence seq) =
    foldrLayer [] fun init seq


foldrLayer prefix fun init layer =
    Layer.foldr
        (\i item result ->
            case item of
                Item entry ->
                    let
                        p =
                            case prefix of
                                [] ->
                                    ( i, [] )

                                head :: rest ->
                                    ( head, rest ++ [ i ] )
                    in
                    fun (Path p) entry result

                SubLayer subLayer ->
                    foldrLayer (prefix ++ [ i ]) fun result subLayer
        )
        init
        layer


{-| Lookup an entry at the given path.
-}
get : Path -> Sequence a -> Maybe (Entry a)
get (Path path_) (Sequence seq) =
    getInLayer path_ seq


getInLayer ( head, tail ) layer =
    case tail of
        [] ->
            case Layer.get head layer of
                Nothing ->
                    Nothing

                Just (Item entry) ->
                    Just entry

                Just (SubLayer subLayer) ->
                    getInLayer ( 0, [] ) subLayer

        next :: rest ->
            case Layer.get head layer of
                Nothing ->
                    Nothing

                Just (Item entry) ->
                    -- path with suffix of zeros equals to a path without the suffix
                    if List.sum tail == 0 then
                        Just entry

                    else
                        Nothing

                Just (SubLayer subLayer) ->
                    getInLayer ( next, rest ) subLayer


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
    Dec.list Dec.int
        |> Dec.andThen
            (\list ->
                case list of
                    head :: tail ->
                        ( head, tail ) |> Path |> Dec.succeed

                    [] ->
                        Dec.fail "empty path"
            )


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
        |> Enc.list identity


encodePath (Path ( head, tail )) =
    head
        :: tail
        |> List.map Enc.int
        |> Enc.list identity


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
    Path ( layerSize 0, [] )


{-| The lowest path possible
-}
minPath : Path
minPath =
    Path ( 0, [] )


{-| Create a path (ie. a non-empty list) given its head and tail.
-}
path : Int -> List Int -> Path
path head tail =
    Path ( head, tail )


{-| Compare Paths
-}
comparePath : Path -> Path -> Order
comparePath (Path ( h1, p1 )) (Path ( h2, p2 )) =
    compare (h1 :: p1) (h2 :: p2)


{-| Turn a path into a string.
-}
pathToString : Path -> String
pathToString (Path ( head, tail )) =
    head
        :: tail
        |> List.map String.fromInt
        |> String.join "/"
        |> (\s -> s ++ "/")

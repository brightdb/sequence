module Tests exposing (all, allocData, beforeAfterData, insertData, originX, originY, originZ, seqToString, seqToStringFold, seqToStringReverse)

import Dict
import Expect
import Fuzz exposing (int, list, string, tuple)
import Sequence exposing (..)
import String
import Test exposing (..)
import Tuple exposing (..)


allocData =
    [ ( minPath, maxPath, path 4 [] )
    , ( minPath, path 1 [], path 0 [ 2 ] )
    , ( path 15 [], maxPath, path 15 [ 4 ] )
    , ( path 0 [ 0, 0, 0 ], path 16 [ 32, 64, 128 ], path 0 [ 0, 0, 1 ] )
    , ( path 0 [ 0, 5 ], path 0 [ 0, 10 ], path 0 [ 0, 6 ] )
    , ( path 0 [], path 0 [ 0, 10 ], path 0 [ 0, 3 ] )
    , ( path 0 [ 0, 10 ], path 1 [], path 0 [ 0, 11 ] )
    ]


originX =
    "alice"


originY =
    "bob"


originZ =
    "charles"


type alias InsertData =
    { t : String
    , sequence : Sequence Char
    , ops : List (Op Char)
    , result : String
    }


insertData =
    [ InsertData "insert a" empty [ createInsert originX (path 3 []) 'a' ] "a"
    , InsertData "insert ab"
        empty
        [ createInsert originX (path 3 []) 'a'
        , createInsert originX (path 5 []) 'b'
        ]
        "ab"
    , InsertData "insert ab0"
        empty
        [ createInsert originX (path 3 []) 'a'
        , createInsert originX (path 5 []) 'b'
        , createInsert originX (path 1 []) '0'
        ]
        "0ab"
    , InsertData "insert abcd, double"
        empty
        [ createInsert originX (path 3 []) 'a'
        , createInsert originX (path 5 []) 'b'
        , createInsert originX (path 8 []) 'c'
        , createInsert originX (path 8 [ 4 ]) 'd'
        , createInsert originX (path 3 []) 'a'
        , createInsert originX (path 5 []) 'b'
        , createInsert originX (path 8 []) 'c'
        , createInsert originX (path 8 [ 4 ]) 'd'
        ]
        "abcd"
    , InsertData "insert a, then 0"
        empty
        [ createInsert originX (path 3 []) 'a'
        , createInsert originX (path 3 []) '0'
        ]
        "a"
    , InsertData "insert abc, different layers"
        empty
        [ createInsert originX (path 3 []) 'a'
        , createInsert originX (path 3 [ 5 ]) 'b'
        , createInsert originX (path 5 []) 'c'
        ]
        "abc"
    , InsertData "insert ab, sibling layers"
        empty
        [ createInsert originX (path 0 [ 1 ]) 'a'
        , createInsert originX (path 0 [ 2 ]) 'b'
        ]
        "ab"
    , InsertData "insert abcd, different layers"
        empty
        [ createInsert originX (path 0 [ 0, 0, 2 ]) 'b'
        , createInsert originX (path 0 [ 0, 0, 1 ]) 'a'
        , createInsert originX (path 8 []) 'd'
        , createInsert originX (path 3 [ 4, 7, 50, 20 ]) 'c'
        ]
        "abcd"
    , InsertData "insert abcd, concurrent"
        empty
        [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 []) 'b'
        , createInsert originY (path 1 []) 'c'
        , createInsert originZ (path 1 []) 'b'
        , createInsert originX (path 2 []) 'd'
        ]
        "aBCBd"
    , InsertData "insert abc, and remove c"
        empty
        [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 []) 'b'
        , createInsert originX (path 2 []) 'c'
        , createRemove originX originX (path 2 [])
        ]
        "ab"
    , InsertData "insert abc, and remove b"
        empty
        [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 []) 'b'
        , createInsert originX (path 2 []) 'c'
        , createRemove originX originX (path 1 [])
        ]
        "ac"
    , InsertData "insert abc, and remove a"
        empty
        [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 []) 'b'
        , createInsert originX (path 2 []) 'c'
        , createRemove originX originX (path 0 [])
        ]
        "bc"
    , InsertData "insert abc, and remove nothing"
        empty
        [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 []) 'b'
        , createInsert originX (path 2 []) 'c'
        , createRemove originX originX (path 3 [])
        ]
        "abc"
    , InsertData "insert abcd, concurrent"
        empty
        [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 []) 'b'
        , createInsert originY (path 1 []) 'c'
        , createRemove originX originY (path 1 [])
        ]
        "aB"
    ]


type alias BeforeAfterData =
    { t : String
    , ops : List (Op Char)
    , path1 : Path
    , path2 : Path
    }


beforeAfterData =
    [ BeforeAfterData "same layer"
        [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 []) 'b'
        ]
        (path 1 [])
        (path 0 [])
    , BeforeAfterData "sublayer"
        [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 [ 1 ]) 'b'
        ]
        (path 1 [ 1 ])
        (path 0 [])
    , BeforeAfterData "sublayer 2"
        [ createInsert originX (path 0 [ 1 ]) 'a'
        , createInsert originX (path 1 []) 'b'
        ]
        (path 1 [])
        (path 0 [ 1 ])
    , BeforeAfterData "sibling layer"
        [ createInsert originX (path 0 [ 1 ]) 'a'
        , createInsert originX (path 1 [ 1 ]) 'b'
        ]
        (path 1 [ 1 ])
        (path 0 [ 1 ])
    , BeforeAfterData "same sublayer"
        [ createInsert originX (path 0 [ 1 ]) 'a'
        , createInsert originX (path 0 [ 2 ]) 'b'
        ]
        (path 0 [ 2 ])
        (path 0 [ 1 ])
    , BeforeAfterData "subsublayer"
        [ createInsert originX (path 0 [ 1 ]) 'a'
        , createInsert originX (path 0 [ 1, 2 ]) 'b'
        ]
        (path 0 [ 1, 2 ])
        (path 0 [ 1, 0 ])
    , BeforeAfterData "subsublayer 2"
        [ createInsert originX (path 0 [ 1 ]) 'a'
        , createInsert originX (path 0 [ 2, 2 ]) 'b'
        ]
        (path 0 [ 2, 2 ])
        (path 0 [ 1 ])
    , BeforeAfterData "subsublayer 3"
        [ createInsert originX (path 0 [ 1 ]) 'a'
        , createInsert originX (path 0 [ 5, 3, 4, 1 ]) 'b'
        ]
        (path 0 [ 5, 3, 4, 1 ])
        (path 0 [ 1 ])
    , BeforeAfterData "subsublayer 4"
        [ createInsert originX (path 0 [ 1 ]) 'b'
        , createInsert originX (path 0 [ 0, 3, 4, 1 ]) 'a'
        ]
        (path 0 [ 1 ])
        (path 0 [ 0, 3, 4, 1 ])
    , BeforeAfterData "same subsublayer"
        [ createInsert originX (path 0 [ 3, 2, 7, 10 ]) 'b'
        , createInsert originX (path 0 [ 0, 3, 4, 1 ]) 'a'
        ]
        (path 0 [ 3, 2, 7, 10 ])
        (path 0 [ 0, 3, 4, 1 ])
    ]


seqToString =
    seqToStringFold foldl


seqToStringReverse =
    seqToStringFold foldr


seqToStringFold fold seq =
    fold
        (\p entry result ->
            let
                _ =
                    Debug.log "seqToSring path" p
            in
            case entry of
                Single _ (Value v) ->
                    result ++ String.fromChar v

                Concurrent mvr ->
                    mvrToList mvr
                        |> List.map second
                        |> List.filterMap
                            (\v ->
                                case v of
                                    Value v_ ->
                                        Just v_

                                    _ ->
                                        Nothing
                            )
                        |> List.map (String.fromChar >> String.toUpper)
                        |> String.concat
                        |> (++) result

                _ ->
                    result
        )
        ""
        seq


testAlloc : Test
testAlloc =
    describe "alloc"
        (List.map
            (\( start, end, pos ) ->
                let
                    title =
                        pathToString start ++ " - " ++ pathToString end ++ " -> " ++ pathToString pos
                in
                test title <|
                    \() ->
                        let
                            _ =
                                Debug.log "" title
                        in
                        Expect.equal (alloc start end) pos
            )
            allocData
        )


testInsert : Test
testInsert =
    describe "insert"
        (List.map
            (\{ t, sequence, ops, result } ->
                let
                    title =
                        t ++ " -> " ++ result
                in
                test title <|
                    \() ->
                        let
                            _ =
                                Debug.log "" title
                        in
                        Expect.equal (apply ops sequence |> (\( seq, _ ) -> seqToString seq)) result
            )
            insertData
        )


testFoldR : Test
testFoldR =
    describe "foldr"
        (List.map
            (\{ t, sequence, ops, result } ->
                let
                    title =
                        "foldr " ++ t ++ " -> " ++ result
                in
                test title <|
                    \() ->
                        let
                            _ =
                                Debug.log "" title
                        in
                        Expect.equal (apply ops sequence |> (\( seq, _ ) -> seqToStringReverse seq)) (String.reverse result)
            )
            []
        )


testBefore : Test
testBefore =
    describe "before"
        (List.map
            (\{ t, ops, path1, path2 } ->
                let
                    sequence =
                        apply ops empty
                            |> Tuple.first

                    title =
                        "before " ++ t ++ ": " ++ pathToString path1 ++ " -> " ++ pathToString path2
                in
                test title <|
                    \() ->
                        let
                            _ =
                                Debug.log "" title

                            _ =
                                Debug.log "sequence" sequence
                        in
                        Expect.equal (before path1 sequence) (Just <| ( path2, Single originX (Value 'a') ))
            )
            beforeAfterData
        )


testAfter : Test
testAfter =
    describe "after"
        (List.map
            (\{ t, ops, path1, path2 } ->
                let
                    sequence =
                        apply ops empty
                            |> Tuple.first

                    title =
                        "after " ++ t ++ ": " ++ pathToString path2 ++ " -> " ++ pathToString path1
                in
                test title <|
                    \() ->
                        let
                            _ =
                                Debug.log "" title

                            _ =
                                Debug.log "sequence" sequence
                        in
                        Expect.equal (after path2 sequence) (Just <| ( path1, Single originX (Value 'b') ))
            )
            beforeAfterData
        )


all : Test
all =
    describe "Sequence"
        [ testAlloc
        , testInsert
        , testBefore
        , testAfter
        ]

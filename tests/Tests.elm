module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Sequence exposing (..)
import Dict
import Tuple exposing (..)


allocData =
    [ ( minPath, maxPath, path 4 [] )
    , ( minPath, path 1 [], path 0 [ 2 ] )
    , ( path 15 [], maxPath, path 15 [ 31 ] )
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


insertData =
    [ ( "insert a", empty, [ createInsert originX (path 3 []) 'a' ], "a" )
    , ( "insert ab"
      , empty
      , [ createInsert originX (path 3 []) 'a'
        , createInsert originX (path 5 []) 'b'
        ]
      , "ab"
      )
    , ( "insert ab0"
      , empty
      , [ createInsert originX (path 3 []) 'a'
        , createInsert originX (path 5 []) 'b'
        , createInsert originX (path 1 []) '0'
        ]
      , "0ab"
      )
    , ( "insert abcd, double"
      , empty
      , [ createInsert originX (path 3 []) 'a'
        , createInsert originX (path 5 []) 'b'
        , createInsert originX (path 8 []) 'c'
        , createInsert originX (path 8 [ 4 ]) 'd'
        , createInsert originX (path 3 []) 'a'
        , createInsert originX (path 5 []) 'b'
        , createInsert originX (path 8 []) 'c'
        , createInsert originX (path 8 [ 4 ]) 'd'
        ]
      , "abcd"
      )
    , ( "insert a, then 0"
      , empty
      , [ createInsert originX (path 3 []) 'a'
        , createInsert originX (path 3 []) '0'
        ]
      , "a"
      )
    , ( "insert abc, different layers"
      , empty
      , [ createInsert originX (path 3 []) 'a'
        , createInsert originX (path 3 [ 5 ]) 'b'
        , createInsert originX (path 5 []) 'c'
        ]
      , "abc"
      )
    , ( "insert ab, sibling layers"
      , empty
      , [ createInsert originX (path 0 [ 1 ]) 'a'
        , createInsert originX (path 0 [ 2 ]) 'b'
        ]
      , "ab"
      )
    , ( "insert abcd, different layers"
      , empty
      , [ createInsert originX (path 0 [ 0, 0, 2 ]) 'b'
        , createInsert originX (path 0 [ 0, 0, 1 ]) 'a'
        , createInsert originX (path 8 []) 'd'
        , createInsert originX (path 3 [ 4, 7, 50, 20 ]) 'c'
        ]
      , "abcd"
      )
    , ( "insert abcd, concurrent"
      , empty
      , [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 []) 'b'
        , createInsert originY (path 1 []) 'c'
        , createInsert originZ (path 1 []) 'b'
        , createInsert originX (path 2 []) 'd'
        ]
      , "aBCBd"
      )
    , ( "insert abc, and remove c"
      , empty
      , [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 []) 'b'
        , createInsert originX (path 2 []) 'c'
        , createRemove originX originX (path 2 [])
        ]
      , "ab"
      )
    , ( "insert abc, and remove b"
      , empty
      , [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 []) 'b'
        , createInsert originX (path 2 []) 'c'
        , createRemove originX originX (path 1 [])
        ]
      , "ac"
      )
    , ( "insert abc, and remove a"
      , empty
      , [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 []) 'b'
        , createInsert originX (path 2 []) 'c'
        , createRemove originX originX (path 0 [])
        ]
      , "bc"
      )
    , ( "insert abc, and remove nothing"
      , empty
      , [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 []) 'b'
        , createInsert originX (path 2 []) 'c'
        , createRemove originX originX (path 3 [])
        ]
      , "abc"
      )
    , ( "insert abcd, concurrent"
      , empty
      , [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 []) 'b'
        , createInsert originY (path 1 []) 'c'
        , createRemove originX originY (path 1 [])
        ]
      , "aB"
      )
    ]


beforeAfterData =
    [ ( "same layer"
      , [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 []) 'b'
        ]
      , path 1 []
      , path 0 []
      )
    , ( "sublayer"
      , [ createInsert originX (path 0 []) 'a'
        , createInsert originX (path 1 [ 1 ]) 'b'
        ]
      , path 1 [ 1 ]
      , (path 0 [])
      )
    , ( "sublayer 2"
      , [ createInsert originX (path 0 [ 1 ]) 'a'
        , createInsert originX (path 1 []) 'b'
        ]
      , path 1 []
      , (path 0 [ 1 ])
      )
    , ( "sibling layer"
      , [ createInsert originX (path 0 [ 1 ]) 'a'
        , createInsert originX (path 1 [ 1 ]) 'b'
        ]
      , path 1 [ 1 ]
      , (path 0 [ 1 ])
      )
    , ( "same sublayer"
      , [ createInsert originX (path 0 [ 1 ]) 'a'
        , createInsert originX (path 0 [ 2 ]) 'b'
        ]
      , path 0 [ 2 ]
      , (path 0 [ 1 ])
      )
    , ( "subsublayer"
      , [ createInsert originX (path 0 [ 1 ]) 'a'
        , createInsert originX (path 0 [ 1, 2 ]) 'b'
        ]
      , path 0 [ 1, 2 ]
      , (path 0 [ 1, 0 ])
      )
    , ( "subsublayer 2"
      , [ createInsert originX (path 0 [ 1 ]) 'a'
        , createInsert originX (path 0 [ 2, 2 ]) 'b'
        ]
      , path 0 [ 2, 2 ]
      , (path 0 [ 1 ])
      )
    , ( "subsublayer 3"
      , [ createInsert originX (path 0 [ 1 ]) 'a'
        , createInsert originX (path 0 [ 5, 3, 4, 1 ]) 'b'
        ]
      , path 0 [ 5, 3, 4, 1 ]
      , (path 0 [ 1 ])
      )
    , ( "subsublayer 4"
      , [ createInsert originX (path 0 [ 1 ]) 'b'
        , createInsert originX (path 0 [ 0, 3, 4, 1 ]) 'a'
        ]
      , path 0 [ 1 ]
      , (path 0 [ 0, 3, 4, 1 ])
      )
    , ( "same subsublayer"
      , [ createInsert originX (path 0 [ 3, 2, 7, 10 ]) 'b'
        , createInsert originX (path 0 [ 0, 3, 4, 1 ]) 'a'
        ]
      , path 0 [ 3, 2, 7, 10 ]
      , (path 0 [ 0, 3, 4, 1 ])
      )
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
                                        Value v ->
                                            Just v

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


all : Test
all =
    describe "Sequence"
        [ describe "alloc"
            (List.map
                (\( start, end, pos ) ->
                    let
                        title =
                            (toString start ++ " - " ++ toString end ++ " -> " ++ toString pos)
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
        , describe "insert"
            (List.map
                (\( t, sequence, ops, result ) ->
                    let
                        title =
                            (t ++ " -> " ++ result)
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
        , describe "foldr"
            (List.map
                (\( t, sequence, ops, result ) ->
                    let
                        title =
                            ("foldr " ++ t ++ " -> " ++ result)
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
        , describe "before"
            (List.map
                (\( t, ops, path, result ) ->
                    let
                        sequence =
                            apply ops empty
                                |> Tuple.first

                        title =
                            ("before " ++ t ++ ": " ++ toString path ++ " -> " ++ toString result)
                    in
                        test title <|
                            \() ->
                                let
                                    _ =
                                        Debug.log "" title

                                    _ =
                                        Debug.log "sequence" sequence
                                in
                                    Expect.equal (before path sequence) (Just <| ( result, Single originX (Value 'a') ))
                )
                beforeAfterData
            )
        , describe "after"
            (List.map
                (\( t, ops, result, path ) ->
                    let
                        sequence =
                            apply ops empty
                                |> Tuple.first

                        title =
                            ("after " ++ t ++ ": " ++ toString path ++ " -> " ++ toString result)
                    in
                        test title <|
                            \() ->
                                let
                                    _ =
                                        Debug.log "" title

                                    _ =
                                        Debug.log "sequence" sequence
                                in
                                    Expect.equal (after path sequence) (Just <| ( result, Single originX (Value 'b') ))
                )
                beforeAfterData
            )
        ]

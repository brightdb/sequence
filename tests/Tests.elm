module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Sequence exposing (..)
import Bitwise exposing (..)


layerFromRangeData =
    [ ( 0, 1006632960, 0 )
    , ( 0, 1006632959, 0 )
    , ( 0, 939524096, 0 )
    , ( 0, 67108864, 0 )
    , ( 0, 67108863, 1 )
    , ( 0, 1, 4 )
    , ( 67108863, 67108863, 4 )
    , ( 67108862, 67108863, 4 )
    ]


layerFromPathData =
    [ ( 0, 0 )
    , ( 1006632960, 0 )
    , ( 1006632959, 4 )
    ]


allocData =
    [ ( 0, shiftLeftBy 26 15, shiftLeftBy 26 14 )
    , ( 0, shiftLeftBy 26 1, shiftLeftBy 21 4 )
    , ( 0, shiftLeftBy 30 1 - 1, shiftLeftBy 26 3 )
    , ( shiftLeftBy 26 1, shiftLeftBy 26 2, shiftLeftBy 21 5 + shiftLeftBy 26 1 )
    ]


all : Test
all =
    describe "Sequence"
        [ describe "layerFromRange"
            (List.map
                (\( start, end, layer ) ->
                    let
                        title =
                            (toString start ++ " - " ++ toString end ++ " -> " ++ toString layer)
                    in
                        test title <|
                            \() ->
                                let
                                    _ =
                                        Debug.log "" title
                                in
                                    Expect.equal (layerFromRange start end 0) layer
                )
                []
            )
        , describe "layerFromRange flipped"
            (List.map
                (\( end, start, layer ) ->
                    let
                        title =
                            (toString start ++ " - " ++ toString end ++ " -> " ++ toString layer)
                    in
                        test title <|
                            \() ->
                                let
                                    _ =
                                        Debug.log "" title
                                in
                                    Expect.equal (layerFromRange start end 0) layer
                )
                []
            )
        , describe "layerFromPath"
            (List.map
                (\( start, layer ) ->
                    let
                        title =
                            (toString start ++ " -> " ++ toString layer)
                    in
                        test title <|
                            \() ->
                                let
                                    _ =
                                        Debug.log "" title
                                in
                                    Expect.equal (layerFromPath start maxLayer) layer
                )
                layerFromPathData
            )
        , describe "alloc"
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
        ]

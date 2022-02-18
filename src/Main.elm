module Main exposing (main)

import Html exposing (div, li, text)
import Sequence exposing (..)
import String
import Tuple


main =
    let
        -- Allocate paths (= positions) in the sequence
        -- given a preceding and a following path.
        -- In the beginning we use the minimum and maximum possible path.
        path1 =
            alloc minPath maxPath

        path2 =
            alloc path1 maxPath

        path3 =
            alloc path2 maxPath

        -- This path is concurrent to path2 because it's allocated between the
        -- same pair of preceding and following paths.
        concurrentPath =
            alloc path1 maxPath

        -- insert operations
        ops =
            [ createInsert "me" path1 'x'
            , createInsert "me" path2 'y'
            , createInsert "me" path3 'z'

            -- a concurrent insert operation (would come in from outside)
            , createInsert "you" concurrentPath 'a'
            ]

        getChar value =
            case value of
                Value a ->
                    String.fromChar a
                        |> Just

                Tomb x ->
                    Nothing

        fold path entry lis =
            let
                item =
                    case entry of
                        Single origin (Value char) ->
                            String.fromChar char
                                |> text
                                |> (\x -> [ x ])
                                |> li []

                        -- concurrent values are concatenating
                        -- with commas
                        Concurrent mvr ->
                            mvrToList mvr
                                |> List.filterMap (Tuple.second >> getChar)
                                |> List.intersperse ","
                                |> String.concat
                                |> text
                                |> (\x -> [ x ])
                                |> li []

                        _ ->
                            text ""
            in
            item :: lis
    in
    apply ops empty
        |> Tuple.first
        |> foldr fold []
        |> div []

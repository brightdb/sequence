# Sequence

This is a prototype of a CRDT for sequential data written in Elm.

**Work in progress!** Use with caution.

Implementation stems from Nedelec et al. "LSEQ: an adaptive structure for sequences in distributed collaborative editing" (2013).

## Example

```
module Main exposing (..)

import Sequence exposing (..)
import String
import Tuple
import Html exposing (div, li, text)


main =
    let
        path1 =
            alloc minPath maxPath

        path2 =
            alloc path1 maxPath

        path3 =
            alloc path2 maxPath

        concurrentPath =
            alloc path1 maxPath

        ops =
            [ createInsert "me" path1 'x'
            , createInsert "me" path2 'y'
            , createInsert "me" path3 'z'
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
```

Yields:

* x
* y,a
* z

So `x`, `y` and `z` have been inserted by "me" while `a` has been inserted by "you" concurrently to `y`.

For a collaborative editor demo project as a more elaborated example see [brightdb-text](/brightdb/brightdb-text).

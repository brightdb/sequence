# Sequence

This is a operation-based conflict-free replicated data type (CRDT) for sequential data written in [Elm](https://elm-lang.org). Another name for it is "ElmSEQ".

Its implementation is derived from Nedelec et al. ["LSEQ: an adaptive structure for sequences in distributed collaborative editing" (2013)](https://hal.archives-ouvertes.fr/docs/00/92/16/33/PDF/fp025-nedelec.pdf).

## CRDTs

Imagine Alice and Bob editing the same text document at the same time from different devices -- but both being offline. How could their changes be merged eventually so that both ends yield the same result and the intention of both probably conflicting editions is preserved?

Conflict-free replicated data types (CRDTs) to the rescue! In a mobile world with unreliable network connectivity, CRDTs allow for distributed offline-first applications integrating concurrent changes in a lightweight way. Data replicas are guaranteed to be eventually consistent among peers while keeping the context and order of merged operations. 

## Resources

* [This project's readme](https://github.com/soundcloud/roshi) and [this blog post](http://archagon.net/blog/2018/03/24/data-laced-with-history/#conflict-free-replicated-data-types) explain CRDTs more in-depth.
* I [gave a talk](https://www.youtube.com/watch?v=r_QmENb-TAA) on this library and CRDTs in general at [Elm Europe 2018](https://2018.elmeurope.org). 

## Installation

This library is for Elm programs. So I assume you have [Elm](https://elm-lang.org) installed. Then install this lib in your project through:

    elm install brightdb/sequence

## Example

This example demonstrates an example use case of this library. First, we create concurrent operations by hand. Then we apply them to the data structure. In a real distributed environment operations would be created by peers and propagated to each other.

Here we are dealing with text as a sequence of characters. However, the CRDT can be applied to sequences (ordered lists) of any other type (eg. todos).

```elm
module Main exposing (main)

import Sequence exposing (..)
import String
import Tuple
import Html exposing (div, li, text)


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
```

Yields:

* x
* y,a
* z

So `x`, `y` and `z` have been inserted by "me" while `a` has been inserted by "you" concurrently to `y`.


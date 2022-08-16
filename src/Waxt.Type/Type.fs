[<AutoOpen>]
module Waxt.Type.Type

open Thoth.Json.Net
open Waxt.Location

type Type =
    | Unit of option<Range>
    | I32 of option<Range>
    | I64 of option<Range>

module Type =
    let tryGetRange =
        function
        | Unit range
        | I32 range
        | I64 range -> range

    let toString =
        function
        | Unit _ -> "unit"
        | I32 _ -> "i32"
        | I64 _ -> "i64"

    let toJson =
        function
        | Unit range ->
            let at =
                match range with
                | Some range -> [ "at", Encode.string (Range.toString range) ]
                | None -> []

            Encode.object
            <| [ "type", Encode.string "unit" ] @ at

        | I32 range ->
            let at =
                match range with
                | Some range -> [ "at", Encode.string (Range.toString range) ]
                | None -> []

            Encode.object
            <| [ "type", Encode.string "i32" ] @ at

        | I64 range ->
            let at =
                match range with
                | Some range -> [ "at", Encode.string (Range.toString range) ]
                | None -> []

            Encode.object
            <| [ "type", Encode.string "i64" ] @ at

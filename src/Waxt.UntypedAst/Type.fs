[<AutoOpen>]
module Waxt.UntypedAst.Type

open Thoth.Json.Net

type Type =
    | Unit
    | I32
    | F64

module Type =
    let toString =
        function
        | Unit -> "unit"
        | I32 -> "i32"
        | F64 -> "f64"

    let toJson = toString >> Encode.string

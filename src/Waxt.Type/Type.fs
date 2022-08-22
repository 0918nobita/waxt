[<AutoOpen>]
module Waxt.Type.Type

open Thoth.Json.Net

type Type =
    | Unit
    | I32
    | I64

module Type =
    let equal (a: Type) (b: Type) : bool =
        match a, b with
        | Unit, Unit -> true
        | I32, I32 -> true
        | I64, I64 -> true
        | _ -> false

    let toString =
        function
        | Unit _ -> "unit"
        | I32 _ -> "i32"
        | I64 _ -> "i64"

    let toJson = toString >> Encode.string

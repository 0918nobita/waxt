module Waxt.Parser.ParseType

open Waxt.Location
open Waxt.Type

let parseType (range: Range) =
    function
    | "unit" -> Ok Unit
    | "i32" -> Ok I32
    | "i64" -> Ok I64
    | ty -> Error(ParseError($"Invalid type `%s{ty}`", range))

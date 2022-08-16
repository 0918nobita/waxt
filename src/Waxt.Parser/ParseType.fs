module Waxt.Parser.ParseType

open Waxt.Location
open Waxt.Type

let parseType (range: Range) =
    function
    | "unit" -> Ok(Unit(Some range))
    | "i32" -> Ok(I32(Some range))
    | "i64" -> Ok(I64(Some range))
    | ty -> Error(ParseError($"Invalid type `%s{ty}`", range))

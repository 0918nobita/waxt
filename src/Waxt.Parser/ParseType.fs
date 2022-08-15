module Waxt.Parser.ParseType

open Waxt.Location
open Waxt.UntypedAst

let parseType (range: Range) =
    function
    | "unit" -> Ok Unit
    | "i32" -> Ok I32
    | "f64" -> Ok F64
    | ty -> Error(ParseError($"Invalid type `%s{ty}`", range))

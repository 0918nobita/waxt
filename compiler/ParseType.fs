module Waxt.Compiler.ParseType

let parseType (range: Range) =
    function
    | "unit" -> Ok Type.Unit
    | "i32" -> Ok Type.I32
    | "f64" -> Ok Type.F64
    | ty -> Error(ParseError($"Invalid type `%s{ty}`", range))

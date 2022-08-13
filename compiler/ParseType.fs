module Waxt.Compiler.ParseType

let parseType (range: Range) =
    function
    | "i32" -> Ok I32
    | ty -> Error(ParseError($"Invalid type `%s{ty}`", range))

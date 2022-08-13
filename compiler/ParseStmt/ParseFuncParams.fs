module Waxt.Compiler.ParseStmt.ParseFuncParams

open FsToolkit.ErrorHandling
open Waxt.Compiler

let rec parseFuncParams: list<SExpr> -> ParseResult<list<string * Ty>> =
    function
    | [] -> Ok []

    | [ Atom (_, (Range (_, ``end``))) ] ->
        Error(ParseError("Expected type name, but reached last element of list", Range.fromPos ``end``))

    | Atom (param, _) :: Atom (ty, tyRange) :: rest ->
        result {
            let! ty = ParseType.parseType tyRange ty
            let! parameters = parseFuncParams rest
            return (param, ty) :: parameters
        }

    | Atom _ :: expr :: _ ->
        let str = SExpr.toString expr
        Error(ParseError($"Expected type name, but got `%s{str}`", (expr :> ILocatable).Locate()))

    | expr :: _ ->
        let str = SExpr.toString expr
        Error(ParseError($"Expected parameter name, but got `%s{str}`", (expr :> ILocatable).Locate()))

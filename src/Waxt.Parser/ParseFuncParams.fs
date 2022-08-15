module Waxt.Parser.ParseFuncParams

open FsToolkit.ErrorHandling
open Waxt.Location
open Waxt.UntypedAst

let rec parseFuncParams: list<SExpr> -> ParseResult<list<string * Type>> =
    function
    | [] -> Ok []

    | [ Atom (_, range) ] ->
        Error(ParseError("Expected type name, but reached last element of list", Range.fromPos (Range.``end`` range)))

    | Atom (param, _) :: Atom (ty, range) :: rest ->
        result {
            let! ty = ParseType.parseType range ty
            let! parameters = parseFuncParams rest
            return (param, ty) :: parameters
        }

    | Atom _ :: expr :: _ ->
        let str = SExpr.toString expr
        Error(ParseError($"Expected type name, but got `%s{str}`", (expr :> ILocatable).Locate()))

    | expr :: _ ->
        let str = SExpr.toString expr
        Error(ParseError($"Expected parameter name, but got `%s{str}`", (expr :> ILocatable).Locate()))

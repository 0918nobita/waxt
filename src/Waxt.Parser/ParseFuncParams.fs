module Waxt.Parser.ParseFuncParams

open FsToolkit.ErrorHandling
open Waxt.Location
open Waxt.Type

let rec parseFuncParams: list<SExpr> -> ParseResult<list<(string * Range) * (Type * Range)>> =
    function
    | [] -> Ok []

    | [ Atom (_, range) ] ->
        Error(ParseError("Expected type name, but reached last element of list", Range.fromPos (Range.``end`` range)))

    | Atom (param, paramRange) :: Atom (ty, tyRange) :: rest ->
        result {
            let! ty = ParseType.parseType tyRange ty
            let! parameters = parseFuncParams rest
            return ((param, paramRange), (ty, tyRange)) :: parameters
        }

    | Atom _ :: expr :: _ ->
        let str = SExpr.toString expr
        Error(ParseError($"Expected type name, but got `%s{str}`", (expr :> ILocatable).Locate()))

    | expr :: _ ->
        let str = SExpr.toString expr
        Error(ParseError($"Expected parameter name, but got `%s{str}`", (expr :> ILocatable).Locate()))

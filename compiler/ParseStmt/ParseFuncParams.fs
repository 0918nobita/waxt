module Waxt.Compiler.ParseStmt.ParseFuncParams

open FsToolkit.ErrorHandling
open Waxt.Compiler.Location
open Waxt.Compiler.Parse
open Waxt.Compiler.SExpr
open Waxt.Compiler.Stmt

let rec parseFuncParams: list<SExpr> -> ParseResult<list<string * Ty>> =
    function
    | [] -> Ok []

    | [ Atom (_, (Range (_, ``end``))) ] -> Error(ParseError("Expected type name", Range.fromPos ``end``))

    | [ expr ] ->
        let (Range (_, ``end``)) = (expr :> ILocatable).Locate()
        Error(ParseError("Expected parameter name", Range.fromPos ``end``))

    | Atom (param, _) :: Atom (ty, tyRange) :: rest ->
        match ty with
        | Type ty ->
            result {
                let! parameters = parseFuncParams rest
                return (param, ty) :: parameters
            }
        | _ -> Error(ParseError($"The parameter type `%s{ty}` is invalid", tyRange))

    | Atom _ :: expr :: _ ->
        let str = SExpr.toString expr
        Error(ParseError($"Expected type name, but got `%s{str}`", (expr :> ILocatable).Locate()))

    | expr :: _ ->
        let str = SExpr.toString expr
        Error(ParseError($"Expected parameter name, but got `%s{str}`", (expr :> ILocatable).Locate()))

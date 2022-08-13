module Waxt.Compiler.ParseStmt.ParseFunc

open FsToolkit.ErrorHandling
open Waxt.Compiler.Location
open Waxt.Compiler.Parse
open Waxt.Compiler.SExpr
open Waxt.Compiler.Stmt

open ParseExpr
open ParseFuncParams

let parseFunc (basePos: Pos) : list<SExpr> -> ParseResult<Stmt> =
    function
    | [] -> Error(ParseError("Expected function name, but reached last element of list", Range.fromPos basePos))

    | [ Atom (_, Range (_, ``end``)) ] -> Error(ParseError("Expected type name or parameters", Range.fromPos ``end``))

    | Atom (name, _) :: Atom (resultTy, range) :: BracketList (parameters, _) :: body ->
        match resultTy with
        | Type ty ->
            result {
                let! parameters = parseFuncParams parameters
                let! body = parseManyExpr body
                return FuncDef(name, Some ty, parameters, body)
            }
        | _ -> Error(ParseError("The result type `%s{result}` is invalid", range))

    | Atom (name, _) :: BracketList (parameters, _) :: body ->
        result {
            let! parameters = parseFuncParams parameters
            let! body = parseManyExpr body
            return FuncDef(name, None, parameters, body)
        }

    | Atom _ :: sExpr :: _ ->
        let str = SExpr.toString sExpr
        Error(ParseError($"Expected type name or parameters, but got `%s{str}`", (sExpr :> ILocatable).Locate()))

    | sExpr :: _ ->
        let str = SExpr.toString sExpr
        Error(ParseError($"Expected function name, but got `%s{str}`", (sExpr :> ILocatable).Locate()))

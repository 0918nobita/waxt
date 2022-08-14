module Waxt.Compiler.ParseStmt.ParseFunc

open FsToolkit.ErrorHandling
open Waxt.Compiler

open ParseExpr
open ParseFuncParams

let parseFunc (basePos: Pos) : list<SExpr> -> ParseResult<Stmt> =
    function
    | [] -> Error(ParseError("Expected function name, but reached last element of list", Range.fromPos basePos))

    | [ Atom (_, Range (_, ``end``)) ] -> Error(ParseError("Expected type name or parameters", Range.fromPos ``end``))

    | Atom (name, _) :: Atom (resultTy, range) :: BracketList (parameters, _) :: body ->
        result {
            let! ty = ParseType.parseType range resultTy
            let! parameters = parseFuncParams parameters
            let! body = parseManyExpr body
            return FuncDef(name, ty, parameters, body)
        }

    | Atom (name, _) :: BracketList (parameters, _) :: body ->
        result {
            let! parameters = parseFuncParams parameters
            let! body = parseManyExpr body
            return FuncDef(name, Type.Unit, parameters, body)
        }

    | Atom _ :: sExpr :: _ ->
        let str = SExpr.toString sExpr
        Error(ParseError($"Expected type name or parameters, but got `%s{str}`", (sExpr :> ILocatable).Locate()))

    | sExpr :: _ ->
        let str = SExpr.toString sExpr
        Error(ParseError($"Expected function name, but got `%s{str}`", (sExpr :> ILocatable).Locate()))

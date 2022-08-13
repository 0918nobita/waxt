module Waxt.Compiler.ParseStmt

open Location
open Parse
open SExpr
open Stmt

let (|Type|_|) (str: string) =
    match str with
    | "i32" -> Some I32
    | _ -> None

open FsToolkit.ErrorHandling

let rec parseFuncParams (exprs: list<SExpr>) =
    match exprs with
    | [] -> Ok []

    | [ Atom ((Range (_, ``end``)), _) ] -> Error(ParseError("Expected type name", Range.fromPos ``end``))

    | [ expr ] ->
        let (Range (_, ``end``)) = (expr :> ILocatable).Locate()
        Error(ParseError("Expected parameter name", Range.fromPos ``end``))

    | Atom (_, param) :: Atom (tyRange, ty) :: rest ->
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

let parseFunc (basePos: Pos) : list<SExpr> -> ParseResult<Stmt> =
    function
    | [] -> Error(ParseError("Expected function name, but reached last element of list", Range.fromPos basePos))

    | Atom (Range (_, ``end``), _) :: [] -> Error(ParseError("Expected type name or parameters", Range.fromPos ``end``))

    | Atom (_, name) :: Atom (range, resultTy) :: BracketList (_, parameters) :: body ->
        // TODO: 本体を求めて AST に反映する
        match resultTy with
        | Type ty ->
            result {
                let! parameters = parseFuncParams parameters
                return FuncDef(name, Some ty, parameters, [])
            }
        | _ -> Error(ParseError("The result type `%s{result}` is invalid", range))

    | Atom (_, name) :: BracketList (_, parameters) :: body ->
        // TODO: 本体を求めて AST に反映する
        result {
            let! parameters = parseFuncParams parameters
            return FuncDef(name, None, parameters, [])
        }

    | Atom _ :: sExpr :: _ ->
        let str = SExpr.toString sExpr
        Error(ParseError($"Expected type name or parameters, but got `%s{str}`", (sExpr :> ILocatable).Locate()))

    | sExpr :: _ ->
        let str = SExpr.toString sExpr
        Error(ParseError($"Expected function name, but got `%s{str}`", (sExpr :> ILocatable).Locate()))

let parseStmt: SExpr -> ParseResult<Stmt> =
    function
    | (BracketList _
    | Atom _) as sExpr ->
        let str = SExpr.toString sExpr
        Error(ParseError($"Expected (...) list, but got `%s{str}`", (sExpr :> ILocatable).Locate()))

    | ParenList (Range (_, ``end``), []) -> Error(ParseError("Expected keyword", Range.fromPos ``end``))

    | ParenList (_, Atom (Range (_, ``end``), "func") :: rest) -> parseFunc ``end`` rest

    | ParenList (_, Atom (range, str) :: _) -> Error(ParseError($"Invalid keyword `%s{str}`", range))

    | ParenList (_, sExpr :: _) ->
        let str = SExpr.toString sExpr
        Error(ParseError($"Expected keyword, but got `%s{str}`", (sExpr :> ILocatable).Locate()))

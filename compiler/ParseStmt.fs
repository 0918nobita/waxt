module Waxt.Compiler.ParseStmt

open Location
open Parse
open SExpr
open Stmt

let (|Type|_|) (str: string) =
    match str with
    | "i32" -> Some I32
    | _ -> None

let parseFunc (basePos: Pos) : list<SExpr> -> ParseResult<Stmt> =
    function
    | [] -> Error(ParseError("Expected function name, but reached last element of list", Range.fromPos basePos))

    | Atom (Range (_, ``end``), _) :: [] -> Error(ParseError("Expected type name or parameters", Range.fromPos ``end``))

    | Atom (_, name) :: Atom (range, result) :: BracketList (_, parameters) :: body ->
        // TODO: パラメータ、本体を求めて AST に反映する
        match result with
        | Type ty -> Ok(FuncDef(name, Some ty, [], []))
        | _ -> Error(ParseError("The result type `%s{result}` is invalid", range))

    | Atom (_, name) :: BracketList (_, parameters) :: body ->
        // TODO: パラメータ、本体を求めて AST に反映する
        Ok(FuncDef(name, None, [], []))

    | Atom _ :: sExpr :: _ ->
        Error(
            ParseError(
                (let sExpr = SExpr.toString sExpr in $"Expected type name or parameters, but got `%s{sExpr}`"),
                (sExpr :> ILocatable).Locate()
            )
        )

    | sExpr :: _ ->
        Error(
            ParseError(
                (let sExpr = SExpr.toString sExpr in $"Expected function name, but got `%s{sExpr}`"),
                (sExpr :> ILocatable).Locate()
            )
        )

let parseStmt: SExpr -> ParseResult<Stmt> =
    function
    | (BracketList _
    | Atom _) as sExpr ->
        Error(
            ParseError(
                (let sExpr = SExpr.toString sExpr in $"Expected (...) list, but got `%s{sExpr}`"),
                (sExpr :> ILocatable).Locate()
            )
        )

    | ParenList (Range (_, ``end``), []) -> Error(ParseError("Expected keyword", Range.fromPos ``end``))

    | ParenList (_, Atom (Range (_, ``end``), "func") :: rest) -> parseFunc ``end`` rest

    | ParenList (_, Atom (range, str) :: _) -> Error(ParseError($"Invalid keyword `%s{str}`", range))

    | ParenList (_, sExpr :: _) ->
        Error(
            ParseError(
                (let sExpr = SExpr.toString sExpr in $"Expected keyword, but got `%s{sExpr}`"),
                (sExpr :> ILocatable).Locate()
            )
        )

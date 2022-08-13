[<AutoOpen>]
module Waxt.Compiler.ParseStmt.Library

open Waxt.Compiler.Location
open Waxt.Compiler.Parse
open Waxt.Compiler.SExpr
open Waxt.Compiler.Stmt

let parseStmt: SExpr -> ParseResult<Stmt> =
    function
    | (BracketList _
    | Atom _) as sExpr ->
        let str = SExpr.toString sExpr
        Error(ParseError($"Expected (...) list, but got `%s{str}`", (sExpr :> ILocatable).Locate()))

    | ParenList ([], Range (_, ``end``)) -> Error(ParseError("Expected keyword", Range.fromPos ``end``))

    | ParenList (Atom ("func", Range (_, ``end``)) :: rest, _) -> ParseFunc.parseFunc ``end`` rest

    | ParenList (Atom (str, range) :: _, _) -> Error(ParseError($"Invalid keyword `%s{str}`", range))

    | ParenList (sExpr :: _, _) ->
        let str = SExpr.toString sExpr
        Error(ParseError($"Expected keyword, but got `%s{str}`", (sExpr :> ILocatable).Locate()))

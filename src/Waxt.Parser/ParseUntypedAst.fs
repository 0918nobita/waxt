module Waxt.Parser.ParseUntypedAst

open Waxt.Location
open Waxt.UntypedAst

let parseStmt: SExpr -> ParseResult<Stmt> =
    function
    | (BracketList _
    | Atom _) as sExpr ->
        let str = SExpr.toString sExpr
        Error(ParseError($"Expected (...) list, but got `%s{str}`", (sExpr :> ILocatable).Locate()))

    | ParenList ([], range) -> Error(ParseError("Expected keyword", Range.fromPos (Range.``end`` range)))

    | ParenList (Atom ("func", range) :: rest, _) -> ParseFunc.parseFunc (Range.``end`` range) rest

    | ParenList (Atom (str, range) :: _, _) -> Error(ParseError($"Invalid keyword `%s{str}`", range))

    | ParenList (sExpr :: _, _) ->
        let str = SExpr.toString sExpr
        Error(ParseError($"Expected keyword, but got `%s{str}`", (sExpr :> ILocatable).Locate()))

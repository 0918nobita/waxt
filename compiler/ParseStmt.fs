module Waxt.Compiler.ParseStmt

open SExpr
open Stmt

type ParseError =
    | ExpectedFuncName of actual: option<SExpr>
    | ExpectedKeyword of actual: option<SExpr>
    | ExpectedSignature of actual: option<SExpr>
    | ExpectedParenList of actual: SExpr
    | InvalidKeyword of actual: string

module ParseError =
    let toString =
        function
        | ExpectedFuncName (Some actual) ->
            let actual = SExpr.toString actual
            $"Expected function name, but found \"%s{actual}\""

        | ExpectedFuncName None -> "Expected function name"

        | ExpectedKeyword (Some actual) ->
            let actual = SExpr.toString actual
            $"Expected \"func\" keyword, but found \"%s{actual}\""

        | ExpectedKeyword None -> "Expected \"func\" keyword"

        | ExpectedSignature (Some actual) ->
            let actual = SExpr.toString actual
            $"Expected type name or parameters, but found \"%s{actual}\""

        | ExpectedSignature None -> "Expected type name or parameters"

        | ExpectedParenList actual ->
            let actual = SExpr.toString actual
            $"Expected parenthesis list, but found \"%s{actual}\""

        | InvalidKeyword actual -> $"Invalid keyword \"%s{actual}\" (possible keywords: \"func\")"

let parseFunc: list<SExpr> -> Result<Stmt, ParseError> =
    function
    | [] -> Error(ExpectedFuncName None)
    | Atom _ :: [] -> Error(ExpectedSignature None)

    | Atom name :: Atom result :: BracketList parameters :: body ->
        // TODO: 戻り値の型、パラメータ、本体を求めて AST に反映する
        Ok(FuncDecl(name, None, [], []))

    | Atom name :: BracketList parameters :: body ->
        // TODO: パラメータ、本体を求めて AST に反映する
        Ok(FuncDecl(name, None, [], []))

    | Atom _ :: sExpr :: _ -> Error(ExpectedSignature(Some sExpr))
    | sExpr :: _ -> Error(ExpectedFuncName(Some sExpr))

let parseStmt: SExpr -> Result<Stmt, ParseError> =
    function
    | (BracketList _
    | Atom _) as sExpr -> Error(ExpectedParenList sExpr)

    | ParenList [] -> Error(ExpectedKeyword None)

    | ParenList (Atom "func" :: rest) -> parseFunc rest

    | ParenList (Atom str :: _) -> Error(InvalidKeyword str)
    | ParenList (sExpr :: _) -> Error(ExpectedKeyword(Some sExpr))

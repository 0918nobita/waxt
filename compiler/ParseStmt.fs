module Waxt.Compiler.ParseStmt

open Location
open Parse
open SExpr
open Stmt

let (|Type|_|) (str: string) =
    match str with
    | "i32" -> Some I32
    | _ -> None

let parseFunc (basePos: Point) : list<SExpr> -> ParseResult<Stmt> =
    function
    | [] ->
        Error
            { new IParseError with
                member _.Msg = "Expected function name"
              interface ILocatable with
                  member _.Locate() = Range.fromPoint basePos }

    | Atom (Range (_, ``end``), _) :: [] ->
        Error
            { new IParseError with
                member _.Msg = "Expected type name or parameters"
              interface ILocatable with
                  member _.Locate() = Range.fromPoint ``end`` }

    | Atom (_, name) :: Atom (resultRange, result) :: BracketList (_, parameters) :: body ->
        // TODO: パラメータ、本体を求めて AST に反映する
        match result with
        | Type ty -> Ok(FuncDecl(name, Some ty, [], []))
        | _ ->
            Error
                { new IParseError with
                    member _.Msg = $"The result type %s{result} is invalid"
                  interface ILocatable with
                      member _.Locate() = resultRange }

    | Atom (_, name) :: BracketList (_, parameters) :: body ->
        // TODO: パラメータ、本体を求めて AST に反映する
        Ok(FuncDecl(name, None, [], []))

    | Atom _ :: sExpr :: _ ->
        Error
            { new IParseError with
                member _.Msg =
                    let sExpr = SExpr.toString sExpr
                    $"Expected type name or parameters, but got %s{sExpr}"
              interface ILocatable with
                  member _.Locate() = (sExpr :> ILocatable).Locate() }

    | sExpr :: _ ->
        Error
            { new IParseError with
                member _.Msg =
                    let sExpr = SExpr.toString sExpr
                    $"Expected function name, but got %s{sExpr}"
              interface ILocatable with
                  member _.Locate() = (sExpr :> ILocatable).Locate() }

let parseStmt: SExpr -> Result<Stmt, IParseError> =
    function
    | (BracketList _
    | Atom _) as sExpr ->
        Error
            { new IParseError with
                member _.Msg =
                    let sExpr = SExpr.toString sExpr
                    $"Expected (...) list, but got %s{sExpr}"
              interface ILocatable with
                  member _.Locate() = (sExpr :> ILocatable).Locate() }

    | ParenList (Range (_, ``end``), []) ->
        Error
            { new IParseError with
                member _.Msg = "Expected keyword"
              interface ILocatable with
                  member _.Locate() = Range.fromPoint ``end`` }

    | ParenList (_, Atom (Range (_, ``end``), "func") :: rest) -> parseFunc ``end`` rest

    | ParenList (_, Atom (range, str) :: _) ->
        Error
            { new IParseError with
                member _.Msg = $"Invalid keyword %s{str} (possible keywords: func)"
              interface ILocatable with
                  member _.Locate() = range }

    | ParenList (_, sExpr :: _) ->
        Error
            { new IParseError with
                member _.Msg =
                    let sExpr = SExpr.toString sExpr
                    $"Expected keyword, but got %s{sExpr}"
              interface ILocatable with
                  member _.Locate() = (sExpr :> ILocatable).Locate() }

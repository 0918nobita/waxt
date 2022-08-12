module Waxt.Compiler.ParseStmt

open Location
open SExpr
open Stmt

type IParseError =
    abstract member Msg: string
    inherit ILocatable

let parseFunc (basePos: Point) : list<SExpr> -> Result<Stmt, IParseError> =
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

    | Atom (_, name) :: Atom (_, result) :: BracketList (_, parameters) :: body ->
        // TODO: 戻り値の型、パラメータ、本体を求めて AST に反映する
        Ok(FuncDecl(name, None, [], []))

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

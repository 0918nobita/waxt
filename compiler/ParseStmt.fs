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

let (|Int|_|) (str: string) =
    match System.Int32.TryParse str with
    | true, value -> Some value
    | _ -> None

let (|I32Literal|_|) (sExpr: SExpr) =
    match sExpr with
    | Atom (Int value, at) -> Some(I32Const(value, at))
    | _ -> None

let rec parseExpr: SExpr -> ParseResult<Expr> =
    function
    | I32Literal expr -> Ok expr
    | Atom (str, at) -> Ok(Var(name = str, at = at))
    | ParenList ([ Atom ("i32.add", _); lhs; rhs ], at) ->
        result {
            let! lhs = parseExpr lhs
            let! rhs = parseExpr rhs
            return I32Add(lhs, rhs, at)
        }
    | ParenList ([ Atom ("i32.mul", _); lhs; rhs ], at) ->
        result {
            let! lhs = parseExpr lhs
            let! rhs = parseExpr rhs
            return I32Mul(lhs, rhs, at)
        }
    | ParenList ([ Atom ("i32.store", _); addr; content ], at) ->
        result {
            let! addr = parseExpr addr
            let! content = parseExpr content
            return I32Store(addr, content, at)
        }
    | sExpr -> Error(ParseError("Invalid expression", (sExpr :> ILocatable).Locate()))

let rec parseManyExpr: list<SExpr> -> ParseResult<list<Expr>> =
    function
    | [] -> Ok []
    | sExpr :: rest ->
        result {
            let! expr = parseExpr sExpr
            let! exprs = parseManyExpr rest
            return expr :: exprs
        }

let parseFunc (basePos: Pos) : list<SExpr> -> ParseResult<Stmt> =
    function
    | [] -> Error(ParseError("Expected function name, but reached last element of list", Range.fromPos basePos))

    | Atom (_, Range (_, ``end``)) :: [] -> Error(ParseError("Expected type name or parameters", Range.fromPos ``end``))

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

let parseStmt: SExpr -> ParseResult<Stmt> =
    function
    | (BracketList _
    | Atom _) as sExpr ->
        let str = SExpr.toString sExpr
        Error(ParseError($"Expected (...) list, but got `%s{str}`", (sExpr :> ILocatable).Locate()))

    | ParenList ([], Range (_, ``end``)) -> Error(ParseError("Expected keyword", Range.fromPos ``end``))

    | ParenList (Atom ("func", Range (_, ``end``)) :: rest, _) -> parseFunc ``end`` rest

    | ParenList (Atom (str, range) :: _, _) -> Error(ParseError($"Invalid keyword `%s{str}`", range))

    | ParenList (sExpr :: _, _) ->
        let str = SExpr.toString sExpr
        Error(ParseError($"Expected keyword, but got `%s{str}`", (sExpr :> ILocatable).Locate()))

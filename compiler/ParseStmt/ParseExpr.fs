module Waxt.Compiler.ParseStmt.ParseExpr

open FsToolkit.ErrorHandling
open Waxt.Compiler

let private (|Int|_|) (str: string) =
    match System.Int32.TryParse str with
    | true, value -> Some value
    | _ -> None

let private (|I32Literal|_|) (sExpr: SExpr) =
    match sExpr with
    | Atom (Int value, at) -> Some(I32Const(value, at))
    | _ -> None

let rec private parseExpr: SExpr -> ParseResult<Expr> =
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

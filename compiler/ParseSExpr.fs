module Waxt.Compiler.ParseSExpr

open FsToolkit.ErrorHandling
open SExpr
open Token

type ParseError =
    | UnexpectedEndOfInput
    | UnexpectedRightParen
    | UnexpectedRightBracket
    | ExpectedRightParen
    | ExpectedRightBracket

type RestOfTokens = list<Token>
type ParseSuccess<'T> = 'T * RestOfTokens

type ParseResult<'T> = Result<ParseSuccess<'T>, ParseError>

let rec parseSExpr: list<Token> -> ParseResult<SExpr> =
    function
    | [] -> Error UnexpectedEndOfInput
    | (Str str) :: rest -> Ok(Atom str, rest)
    | LeftParen :: rest ->
        result {
            match! parseManySExpr rest with
            | (exprs, RightParen :: rest) -> return (ParenList exprs, rest)
            | _ -> return! Error ExpectedRightParen
        }
    | RightParen :: _ -> Error UnexpectedRightParen
    | LeftBracket :: rest ->
        result {
            match! parseManySExpr rest with
            | (exprs, RightBracket :: rest) -> return (BracketList exprs, rest)
            | _ -> return! Error ExpectedRightBracket
        }
    | RightBracket :: _ -> Error UnexpectedRightBracket

and private parseManySExpr: list<Token> -> ParseResult<list<SExpr>> =
    function
    | [] -> Ok([], [])
    | tokens ->
        match parseSExpr tokens with
        | Error _ -> Ok([], tokens)
        | Ok (expr, rest) ->
            result {
                let! (exprs, rest) = parseManySExpr rest
                return (expr :: exprs, rest)
            }

module Waxt.Compiler.Parse

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

let rec parseSExpr (tokens: list<Token>) : ParseResult<SExpr> =
    match tokens with
    | [] -> Error UnexpectedEndOfInput
    | (Str str) :: rest -> Ok(Atom str, rest)
    | LeftParen :: rest ->
        result {
            let! (exprs, rest) = parseManySExpr rest

            return!
                match rest with
                | RightParen :: rest -> Ok(ParenList exprs, rest)
                | _ -> Error ExpectedRightParen
        }
    | RightParen :: _ -> Error UnexpectedRightParen
    | LeftBracket :: rest ->
        result {
            let! (exprs, rest) = parseManySExpr rest

            return!
                match rest with
                | RightBracket :: rest -> Ok(BracketList exprs, rest)
                | _ -> Error ExpectedRightBracket
        }
    | RightBracket :: _ -> Error UnexpectedRightBracket

and parseManySExpr (tokens: list<Token>) : ParseResult<list<SExpr>> =
    match tokens with
    | [] -> Ok([], [])
    | _ ->
        match parseSExpr tokens with
        | Error _ -> Ok([], tokens)
        | Ok (expr, rest) ->
            result {
                let! (exprs, rest) = parseManySExpr rest
                return (expr :: exprs, rest)
            }

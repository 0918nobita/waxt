module Waxt.Compiler.ParseSExpr

open FsToolkit.ErrorHandling
open Location
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
    | (Str (range, str)) :: rest -> Ok(Atom(range, str), rest)
    | LeftParen start :: rest ->
        result {
            match! parseManySExpr rest with
            | (exprs, RightParen ``end`` :: rest) -> return (ParenList(Range(start, ``end``), exprs), rest)
            | _ -> return! Error ExpectedRightParen
        }
    | RightParen _ :: _ -> Error UnexpectedRightParen
    | LeftBracket start :: rest ->
        result {
            match! parseManySExpr rest with
            | (exprs, RightBracket ``end`` :: rest) -> return (BracketList(Range(start, ``end``), exprs), rest)
            | _ -> return! Error ExpectedRightBracket
        }
    | RightBracket _ :: _ -> Error UnexpectedRightBracket

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

module Waxt.Parser.ParseSExpr

open FsToolkit.ErrorHandling
open Waxt.Location
open Waxt.Token

let rec parseSExpr (basePos: Pos) : list<Token> -> ParseResult<SExpr * list<Token>> =
    function
    | [] -> Error(ParseError("Unexpected end of input", Range.fromPos basePos))

    | (Str (content, at)) :: rest -> Ok(Atom(content, at), rest)

    | LeftParen basePos :: rest ->
        result {
            match! parseManySExpr basePos rest with
            | (_, []) -> return! Error(ParseError("Expected `)`, but reached end of input", Range.fromPos basePos))

            | (exprs, RightParen ``end`` :: rest) -> return ParenList(exprs, Range.make basePos ``end``), rest

            | (_, tok :: _) ->
                let range = (tok :> ILocatable).Locate()
                let tok = Token.toString tok
                return! Error(ParseError($"Expected `)`, but got `%s{tok}`", range))
        }

    | LeftBracket start :: rest ->
        result {
            match! parseManySExpr start rest with
            | (exprs, RightBracket ``end`` :: rest) -> return BracketList(exprs, Range.make start ``end``), rest

            | (_, []) -> return! Error(ParseError("Expected `]`, but reached end of input", Range.fromPos basePos))

            | (_, tok :: _) ->
                let range = (tok :> ILocatable).Locate()
                let tok = Token.toString tok
                return! Error(ParseError($"Expected `]`, but got `%s{tok}`", range))
        }

    | tok :: _ ->
        let range = (tok :> ILocatable).Locate()
        let tok = Token.toString tok
        Error(ParseError($"Unexpected token `%s{tok}`", range))

and parseManySExpr (basePos: Pos) : list<Token> -> ParseResult<list<SExpr> * list<Token>> =
    function
    | [] -> Ok([], [])

    | tokens ->
        match parseSExpr basePos tokens with
        | Error _ -> Ok([], tokens)

        | Ok (expr, rest) ->
            let basePos = (expr :> ILocatable).Locate() |> Range.``end``

            result {
                let! (exprs, rest) = parseManySExpr basePos rest
                return (expr :: exprs, rest)
            }

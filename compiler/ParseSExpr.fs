module Waxt.Compiler.ParseSExpr

open FsToolkit.ErrorHandling
open Location
open Parse
open SExpr
open Token

let rec parseSExpr (basePos: Pos) : list<Token> -> ParseResult<SExpr * list<Token>> =
    function
    | [] -> Error(ParseError("Unexpected end of input", Range.fromPos basePos))

    | (Str (range, str)) :: rest -> Ok(Atom(range, str), rest)

    | LeftParen basePos :: rest ->
        result {
            match! parseManySExpr basePos rest with
            | (_, []) -> return! Error(ParseError("Expected `)`, but reached end of input", Range.fromPos basePos))

            | (exprs, RightParen ``end`` :: rest) -> return (ParenList(exprs, Range(basePos, ``end``)), rest)

            | (_, tok :: _) ->
                return!
                    Error(
                        ParseError(
                            (let tok = Token.toString tok in $"Expected `)`, but got `%s{tok}`"),
                            (tok :> ILocatable).Locate()
                        )
                    )
        }

    | LeftBracket start :: rest ->
        result {
            match! parseManySExpr start rest with
            | (exprs, RightBracket ``end`` :: rest) -> return (BracketList(exprs, Range(start, ``end``)), rest)

            | (_, []) -> return! Error(ParseError("Expected `]`, but reached end of input", Range.fromPos basePos))

            | (_, tok :: _) ->
                return!
                    Error(
                        ParseError(
                            (let tok = Token.toString tok in $"Expected `]`, but got `%s{tok}`"),
                            (tok :> ILocatable).Locate()
                        )
                    )
        }

    | tok :: _ ->
        Error(ParseError((let tok = Token.toString tok in $"Unexpected token `%s{tok}`"), (tok :> ILocatable).Locate()))

and private parseManySExpr (basePos: Pos) : list<Token> -> ParseResult<list<SExpr> * list<Token>> =
    function
    | [] -> Ok([], [])

    | tokens ->
        match parseSExpr basePos tokens with
        | Error _ -> Ok([], tokens)

        | Ok (expr, rest) ->
            let (Range (_, basePos)) = (expr :> ILocatable).Locate()

            result {
                let! (exprs, rest) = parseManySExpr basePos rest
                return (expr :: exprs, rest)
            }

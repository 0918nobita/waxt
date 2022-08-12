module Waxt.Compiler.ParseSExpr

open FsToolkit.ErrorHandling
open Location
open Parse
open SExpr
open Token

let rec parseSExpr (basePos: Point) : list<Token> -> ParseResult<SExpr * list<Token>> =
    function
    | [] ->
        Error
            { new IParseError with
                member _.Msg = "Unexpected end of input"
              interface ILocatable with
                  member _.Locate() = Range.fromPoint basePos }

    | (Str (range, str)) :: rest -> Ok(Atom(range, str), rest)

    | LeftParen start :: rest ->
        result {
            match! parseManySExpr start rest with
            | (exprs, RightParen ``end`` :: rest) -> return (ParenList(Range(start, ``end``), exprs), rest)

            | (_, []) ->
                return!
                    Error
                        { new IParseError with
                            member _.Msg = "Expected \")\", but reached end of input"
                          interface ILocatable with
                              member _.Locate() = Range.fromPoint basePos }

            | (_, tok :: _) ->
                return!
                    Error
                        { new IParseError with
                            member _.Msg =
                                let tok = Token.toString tok
                                $"Expected \")\", but got %s{tok}"
                          interface ILocatable with
                              member _.Locate() = (tok :> ILocatable).Locate() }
        }

    | LeftBracket start :: rest ->
        result {
            match! parseManySExpr start rest with
            | (exprs, RightBracket ``end`` :: rest) -> return (BracketList(Range(start, ``end``), exprs), rest)

            | (_, []) ->
                return!
                    Error
                        { new IParseError with
                            member _.Msg = "Expected \"]\", but reached end of input"
                          interface ILocatable with
                              member _.Locate() = Range.fromPoint basePos }

            | (_, tok :: _) ->
                return!
                    Error
                        { new IParseError with
                            member _.Msg =
                                let tok = Token.toString tok
                                $"Expected \"]\", but got %s{tok}"
                          interface ILocatable with
                              member _.Locate() = (tok :> ILocatable).Locate() }
        }

    | tok :: _ ->
        Error
            { new IParseError with
                member _.Msg = "Unexpected token"
              interface ILocatable with
                  member _.Locate() = (tok :> ILocatable).Locate() }

and private parseManySExpr (basePos: Point) : list<Token> -> ParseResult<list<SExpr> * list<Token>> =
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

[<AutoOpen>]
module Waxt.Lexer.Library

open FsToolkit.ErrorHandling
open Waxt.Location
open Waxt.Token

open CharPattern
open State

let private makeI32Lit (neg: bool) (abs: int) (start: Pos) (end_: Pos) =
    Token.I32Lit(I32Lit.make (if neg then -abs else abs) (Range.make start end_))

let private foldStateExtractingToken
    (currentPos: Pos)
    (state: LexState)
    (ifInitial: unit -> Result<'t, list<LexError>>)
    (ifOthers: Token -> Result<'t, list<LexError>>)
    =
    match state with
    | Initial -> ifInitial ()
    | ReadingMinus _ ->
        let end_ = Pos.previousColumn currentPos
        Error [ LexError("Expected digit(s) after minus", end_) ]
    | ReadingI32Lit (startPos, abs, neg) ->
        let end_ = Pos.previousColumn currentPos
        ifOthers (makeI32Lit neg abs startPos end_)
    | ReadingIdent (startPos, raw) ->
        let end_ = Pos.previousColumn currentPos
        ifOthers (Token.Ident(Ident.make raw (Range.make startPos end_)))

let rec private lex'
    (lexOption: LexOption)
    (currentPos: Pos)
    (state: LexState)
    (src: list<char>)
    : Result<list<Token>, list<LexError>> =
    match src with
    | [] -> foldStateExtractingToken currentPos state (fun () -> Ok []) (fun token -> Ok [ token ])

    | NewLine lexOption cs ->
        foldStateExtractingToken
            currentPos
            state
            (fun () -> lex' lexOption (Pos.nextLine currentPos) state cs)
            (fun token ->
                result {
                    let! succeedingTokens = lex' lexOption (Pos.nextLine currentPos) Initial cs
                    return token :: succeedingTokens
                })

    | WhiteSpace :: cs ->
        foldStateExtractingToken
            currentPos
            state
            (fun () -> lex' lexOption (Pos.nextColumn currentPos) Initial cs)
            (fun token ->
                result {
                    let! succeedingTokens = lex' lexOption (Pos.nextColumn currentPos) Initial cs
                    return token :: succeedingTokens
                })

    | (CLeftParen makeTok
    | CRightParen makeTok
    | CLeftBracket makeTok
    | CRightBracket makeTok) :: cs ->
        foldStateExtractingToken
            currentPos
            state
            (fun () ->
                result {
                    let! succeedingTokens = lex' lexOption (Pos.nextColumn currentPos) Initial cs
                    return makeTok currentPos :: succeedingTokens
                })
            (fun token ->
                result {
                    let! succeedingTokens = lex' lexOption (Pos.nextColumn currentPos) Initial cs
                    return token :: makeTok currentPos :: succeedingTokens
                })

    | '-' :: cs ->
        match state with
        | Initial -> lex' lexOption (Pos.nextColumn currentPos) (ReadingMinus currentPos) cs
        | ReadingMinus _ -> Error [ LexError("Expected digits", currentPos) ]
        | ReadingI32Lit _ -> Error [ LexError("Unexpected minus", currentPos) ]
        | ReadingIdent (startPos, raw) ->
            lex' lexOption (Pos.nextColumn currentPos) (ReadingIdent(startPos, raw + "-")) cs

    | Letter as c :: cs ->
        match state with
        | Initial -> lex' lexOption (Pos.nextColumn currentPos) (ReadingIdent(currentPos, string c)) cs
        | ReadingMinus _ -> Error [ LexError("Expected digits", currentPos) ]
        | ReadingI32Lit _ ->
            let errors =
                lex' lexOption (Pos.nextColumn currentPos) Initial cs
                |> Result.defaultError []

            Error(
                LexError("Expected digit, but letter found", currentPos)
                :: errors
            )
        | ReadingIdent (startPos, raw) ->
            lex' lexOption (Pos.nextColumn currentPos) (ReadingIdent(startPos, raw + string c)) cs

    | Digit n :: cs ->
        match state with
        | Initial -> lex' lexOption (Pos.nextColumn currentPos) (ReadingI32Lit(currentPos, n, false)) cs
        | ReadingMinus startPos -> lex' lexOption (Pos.nextColumn currentPos) (ReadingI32Lit(startPos, n, true)) cs
        | ReadingI32Lit (startPos, abs, neg) ->
            lex' lexOption (Pos.nextColumn currentPos) (ReadingI32Lit(startPos, abs * 10 + n, neg)) cs
        | ReadingIdent (startPos, raw) ->
            lex' lexOption (Pos.nextColumn currentPos) (ReadingIdent(startPos, raw + string n)) cs

    | c :: cs ->
        let errors =
            lex' lexOption (Pos.nextColumn currentPos) Initial cs
            |> Result.defaultError []

        Error(
            LexError("Unexpected character: " + string c, currentPos)
            :: errors
        )

let lex (lexOption: LexOption) (src: string) : Result<list<Token>, list<LexError>> =
    lex' lexOption Pos.origin Initial (Seq.toList src)

module Waxt.Lexer

open FsToolkit.ErrorHandling
open Waxt.Location
open Waxt.Token

type Eol =
    | Cr
    | Lf
    | Crlf

type LexOption = LexOption of eol: Eol

let private (|NewLine|_|) (LexOption eol) (cs: list<char>) =
    match eol, cs with
    | Cr, '\r' :: cs' -> Some cs'
    | Lf, '\n' :: cs' -> Some cs'
    | Crlf, '\r' :: '\n' :: cs' -> Some cs'
    | _ -> None

let private (|WhiteSpace|_|) c =
    if System.Char.IsWhiteSpace c then
        Some()
    else
        None

let private (|Digit|_|) c =
    if System.Char.IsDigit c then
        Some(int c - int '0')
    else
        None

let private (|Letter|_|) c =
    if System.Char.IsLetter c then
        Some()
    else
        None

let private (|CLeftParen|_|) =
    function
    | '(' -> Some(fun pos -> Token.LeftParen(LeftParen pos))
    | _ -> None

let private (|CRightParen|_|) =
    function
    | ')' -> Some(fun pos -> Token.RightParen(RightParen pos))
    | _ -> None

let private (|CLeftBracket|_|) =
    function
    | '[' -> Some(fun pos -> Token.LeftBracket(LeftBracket pos))
    | _ -> None

let private (|CRightBracket|_|) =
    function
    | ']' -> Some(fun pos -> Token.RightBracket(RightBracket pos))
    | _ -> None

type LexState =
    | Initial
    | ReadingI32Lit of startPos: Pos * value: int
    | ReadingIdent of startPos: Pos * raw: string

let rec private lex'
    (lexOption: LexOption)
    (currentPos: Pos)
    (state: LexState)
    (src: list<char>)
    : Result<list<Token>, list<string>> =
    match src with
    | [] ->
        match state with
        | Initial -> Ok []
        | ReadingI32Lit (startPos, n) ->
            let end_ = Pos.previousColumn currentPos
            Ok [ Token.I32Lit(I32Lit(n, Range.make startPos end_)) ]
        | ReadingIdent (startPos, raw) ->
            let end_ = Pos.previousColumn currentPos
            Ok [ Token.Ident(Ident.make raw (Range.make startPos end_)) ]

    | NewLine lexOption cs ->
        match state with
        | Initial -> lex' lexOption (Pos.nextLine currentPos) Initial cs
        | ReadingI32Lit (startPos, n) ->
            let end_ = Pos.previousColumn currentPos

            result {
                let! succeedingTokens = lex' lexOption (Pos.nextLine currentPos) Initial cs

                return
                    Token.I32Lit(I32Lit(n, Range.make startPos end_))
                    :: succeedingTokens
            }
        | ReadingIdent (startPos, raw) ->
            let end_ = Pos.previousColumn currentPos

            result {
                let! succeedingTokens = lex' lexOption (Pos.nextLine currentPos) Initial cs

                return
                    Token.Ident(Ident.make raw (Range.make startPos end_))
                    :: succeedingTokens
            }

    | WhiteSpace :: cs ->
        match state with
        | Initial -> lex' lexOption (Pos.nextColumn currentPos) Initial cs
        | ReadingI32Lit (startPos, n) ->
            let end_ = Pos.previousColumn currentPos

            result {
                let! succeedingTokens = lex' lexOption (Pos.nextColumn currentPos) Initial cs

                return
                    Token.I32Lit(I32Lit(n, Range.make startPos end_))
                    :: succeedingTokens
            }
        | ReadingIdent (startPos, raw) ->
            let end_ = Pos.previousColumn currentPos

            result {
                let! succeedingTokens = lex' lexOption (Pos.nextColumn currentPos) Initial cs

                return
                    Token.Ident(Ident.make raw (Range.make startPos end_))
                    :: succeedingTokens
            }

    | (CLeftParen makeTok
    | CRightParen makeTok
    | CLeftBracket makeTok
    | CRightBracket makeTok) :: cs ->
        match state with
        | Initial ->
            result {
                let! succeedingTokens = lex' lexOption (Pos.nextColumn currentPos) Initial cs
                return makeTok currentPos :: succeedingTokens
            }
        | ReadingI32Lit (startPos, n) ->
            let end_ = Pos.previousColumn currentPos

            result {
                let! succeedingTokens = lex' lexOption (Pos.nextColumn currentPos) Initial cs

                return
                    Token.I32Lit(I32Lit(n, Range.make startPos end_))
                    :: makeTok currentPos :: succeedingTokens
            }
        | ReadingIdent (startPos, raw) ->
            let end_ = Pos.previousColumn currentPos

            result {
                let! succeedingTokens = lex' lexOption (Pos.nextColumn currentPos) Initial cs

                return
                    Token.Ident(Ident.make raw (Range.make startPos end_))
                    :: makeTok currentPos :: succeedingTokens
            }

    | Letter as c :: cs ->
        match state with
        | Initial -> lex' lexOption (Pos.nextColumn currentPos) (ReadingIdent(currentPos, string c)) cs
        | ReadingI32Lit _ ->
            let errors =
                lex' lexOption (Pos.nextColumn currentPos) Initial cs
                |> Result.defaultError []

            Error("Expected digit, but letter found" :: errors)
        | ReadingIdent (startPos, raw) ->
            lex' lexOption (Pos.nextColumn currentPos) (ReadingIdent(startPos, raw + string c)) cs

    | Digit n :: cs ->
        match state with
        | Initial -> lex' lexOption (Pos.nextColumn currentPos) (ReadingI32Lit(currentPos, n)) cs
        | ReadingI32Lit (startPos, n') ->
            lex' lexOption (Pos.nextColumn currentPos) (ReadingI32Lit(startPos, n' * 10 + n)) cs
        | ReadingIdent (startPos, raw) ->
            lex' lexOption (Pos.nextColumn currentPos) (ReadingIdent(startPos, raw + string n)) cs

    | c :: cs ->
        let errors =
            lex' lexOption (Pos.nextColumn currentPos) Initial cs
            |> Result.defaultError []

        Error("Unexpected character: " + string c :: errors)

let lex (lexOption: LexOption) (src: string) : Result<list<Token>, list<string>> =
    lex' lexOption Pos.origin Initial (Seq.toList src)

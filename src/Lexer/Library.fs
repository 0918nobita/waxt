module Waxt.Lexer

open Waxt.Location
open Waxt.Token

type Eol =
    | Cr
    | Lf
    | Crlf

type LexOption = LexOption of eol: Eol

let (|NewLine|_|) (LexOption eol) (cs: list<char>) =
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

let private (|CLeftParen|_|) =
    function
    | '(' -> Some LeftParen
    | _ -> None

let private (|CRightParen|_|) =
    function
    | ')' -> Some RightParen
    | _ -> None

let private (|CLeftBracket|_|) =
    function
    | '[' -> Some LeftBracket
    | _ -> None

let private (|CRightBracket|_|) =
    function
    | ']' -> Some RightBracket
    | _ -> None

let rec private lex'
    (lexOption: LexOption)
    (currentPos: Pos)
    (strAcc: option<Pos * string>)
    (src: list<char>)
    : list<Token> =
    match strAcc, src with
    | None, [] -> []

    | None, NewLine lexOption cs -> lex' lexOption (Pos.nextLine currentPos) None cs

    | None, WhiteSpace :: cs -> lex' lexOption (Pos.nextColumn currentPos) None cs

    | None, c :: cs -> lex' lexOption (Pos.nextColumn currentPos) (Some(currentPos, string c)) cs

    | Some (startPos, str), [] -> [ Token.Ident(Ident.make str (Range.make startPos currentPos)) ]

    | Some (startPos, str), NewLine lexOption cs ->
        let tokens = lex' lexOption (Pos.nextLine currentPos) None cs

        Token.Ident(Ident.make str (Range.make startPos (Pos.previousColumn currentPos)))
        :: tokens

    | Some (startPos, str), WhiteSpace :: cs ->
        let tokens = lex' lexOption (Pos.nextColumn currentPos) None cs

        Token.Ident(Ident.make str (Range.make startPos (Pos.previousColumn currentPos)))
        :: tokens

    | Some (startPos, str), c :: cs -> lex' lexOption (Pos.nextColumn currentPos) (Some(startPos, str + string c)) cs

let lex (lexOption: LexOption) (src: string) : list<Token> =
    lex' lexOption Pos.origin None (Seq.toList src)

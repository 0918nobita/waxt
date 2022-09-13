module Waxt.Lexer

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

    | None,
      (CLeftParen makeTok
      | CRightParen makeTok
      | CLeftBracket makeTok
      | CRightBracket makeTok) :: cs ->
        makeTok currentPos
        :: lex' lexOption (Pos.nextColumn currentPos) None cs

    | None, c :: cs -> lex' lexOption (Pos.nextColumn currentPos) (Some(currentPos, string c)) cs

    | Some (startPos, str), [] -> [ Token.Ident(Ident.make str (Range.make startPos currentPos)) ]

    | Some (startPos, str), NewLine lexOption cs ->
        let succeedingTokens = lex' lexOption (Pos.nextLine currentPos) None cs

        Token.Ident(Ident.make str (Range.make startPos (Pos.previousColumn currentPos)))
        :: succeedingTokens

    | Some (startPos, str), WhiteSpace :: cs ->
        let succeedingTokens = lex' lexOption (Pos.nextColumn currentPos) None cs

        Token.Ident(Ident.make str (Range.make startPos (Pos.previousColumn currentPos)))
        :: succeedingTokens

    | Some (startPos, str),
      (CLeftParen makeTok
      | CRightParen makeTok
      | CLeftBracket makeTok
      | CRightBracket makeTok) :: cs ->
        let succeedingTokens = lex' lexOption (Pos.nextColumn currentPos) None cs

        Token.Ident(Ident.make str (Range.make startPos (Pos.previousColumn currentPos)))
        :: makeTok currentPos :: succeedingTokens

    | Some (startPos, str), c :: cs -> lex' lexOption (Pos.nextColumn currentPos) (Some(startPos, str + string c)) cs

let lex (lexOption: LexOption) (src: string) : list<Token> =
    lex' lexOption Pos.origin None (Seq.toList src)

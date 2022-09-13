module private Waxt.Lexer.CharPattern

open Waxt.Token

let (|NewLine|_|) (LexOption eol) (cs: list<char>) =
    match eol, cs with
    | Cr, '\r' :: cs' -> Some cs'
    | Lf, '\n' :: cs' -> Some cs'
    | Crlf, '\r' :: '\n' :: cs' -> Some cs'
    | _ -> None

let (|WhiteSpace|_|) c =
    if System.Char.IsWhiteSpace c then
        Some()
    else
        None

let (|Digit|_|) c =
    if System.Char.IsDigit c then
        Some(int c - int '0')
    else
        None

let (|Letter|_|) c =
    if System.Char.IsLetter c then
        Some()
    else
        None

let (|CLeftParen|_|) =
    function
    | '(' -> Some(fun pos -> Token.LeftParen(LeftParen pos))
    | _ -> None

let (|CRightParen|_|) =
    function
    | ')' -> Some(fun pos -> Token.RightParen(RightParen pos))
    | _ -> None

let (|CLeftBracket|_|) =
    function
    | '[' -> Some(fun pos -> Token.LeftBracket(LeftBracket pos))
    | _ -> None

let (|CRightBracket|_|) =
    function
    | ']' -> Some(fun pos -> Token.RightBracket(RightBracket pos))
    | _ -> None

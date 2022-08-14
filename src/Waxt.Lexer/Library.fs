[<AutoOpen>]
module Waxt.Lexer.Library

open Waxt.Location
open Waxt.Token

type LexerError = LineFeedNotFound of Pos

let (|CLeftParen|_|) =
    function
    | '(' -> Some LeftParen
    | _ -> None

let (|CRightParen|_|) =
    function
    | ')' -> Some RightParen
    | _ -> None

let (|CLeftBracket|_|) =
    function
    | '[' -> Some LeftBracket
    | _ -> None

let (|CRightBracket|_|) =
    function
    | ']' -> Some RightBracket
    | _ -> None

let (|WhiteSpace|_|) c =
    if System.Char.IsWhiteSpace c then
        Some()
    else
        None

open FsToolkit.ErrorHandling

let lex (str: string) : Result<list<Token>, LexerError> =
    let rec inner (basePos: Pos) (strAcc: option<Pos * string>) (chars: list<char>) : Result<list<Token>, LexerError> =
        match strAcc, chars with
        | None, [] -> Ok []

        | Some (start, str), [] -> Ok [ Str(str, Range.make start basePos) ]

        | None, '\r' :: '\n' :: cs -> inner (Pos.nextLine basePos) None cs

        | Some (start, str), '\r' :: '\n' :: cs ->
            result {
                let! tokens = inner (Pos.nextLine basePos) None cs

                return
                    Str(str, Range.make start (Pos.previousCol basePos))
                    :: tokens
            }

        | _, '\r' :: _ -> Error(LineFeedNotFound basePos)

        | None, WhiteSpace :: cs -> inner (Pos.nextCol basePos) None cs

        | Some (start, str), WhiteSpace :: cs ->
            result {
                let! tokens = inner (Pos.nextCol basePos) None cs

                return
                    Str(str, Range.make start (Pos.previousCol basePos))
                    :: tokens
            }

        | None,
          (CLeftParen makeTok
          | CRightParen makeTok
          | CLeftBracket makeTok
          | CRightBracket makeTok) :: cs ->
            result {
                let! tokens = inner (Pos.nextCol basePos) None cs
                return makeTok basePos :: tokens
            }

        | Some (start, str),
          (CLeftParen makeTok
          | CRightParen makeTok
          | CLeftBracket makeTok
          | CRightBracket makeTok) :: cs ->
            result {
                let! tokens = inner (Pos.nextCol basePos) None cs

                return
                    Str(str, Range.make start (Pos.previousCol basePos))
                    :: makeTok basePos :: tokens
            }

        | None, c :: cs -> inner (Pos.nextCol basePos) (Some(basePos, string c)) cs

        | Some (start, str), c :: cs -> inner (Pos.nextCol basePos) (Some(start, string c + str)) cs

    inner Pos.origin None (Seq.toList str)

module Waxt.Compiler.Lex

open Token

type private LexState =
    | Initial
    | ReadingStr of list<char>

let private isWhiteSpace = System.Char.IsWhiteSpace

let private charsToStr = List.rev >> List.toArray >> System.String

let private processFinalState (tokens, state) =
    match state with
    | Initial -> tokens
    | ReadingStr cs -> (Str(charsToStr cs) :: tokens)

let private folder (tokens, state) c =
    match state with
    | Initial ->
        match c with
        | '(' -> (LeftParen :: tokens, Initial)
        | ')' -> (RightParen :: tokens, Initial)
        | '[' -> (LeftBracket :: tokens, Initial)
        | ']' -> (RightBracket :: tokens, Initial)
        | c when isWhiteSpace c -> (tokens, Initial)
        | c -> (tokens, ReadingStr [ c ])
    | ReadingStr cs ->
        match c with
        | '(' -> (LeftParen :: Str(charsToStr cs) :: tokens, Initial)
        | ')' -> (RightParen :: Str(charsToStr cs) :: tokens, Initial)
        | '[' -> (LeftBracket :: Str(charsToStr cs) :: tokens, Initial)
        | ']' -> (RightBracket :: Str(charsToStr cs) :: tokens, Initial)
        | c when isWhiteSpace c -> (Str(charsToStr cs) :: tokens, Initial)
        | c -> (tokens, ReadingStr(c :: cs))

let lex (src: string) =
    src
    |> Seq.fold folder ([], Initial)
    |> processFinalState
    |> List.rev

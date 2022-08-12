module Waxt.Compiler.Lex

open Location
open Token

type private LexState =
    | Initial of currentPos: Pos
    | ReadingStr of currentPos: Pos * start: Pos * list<char>

let private isWhiteSpace = System.Char.IsWhiteSpace

let private charsToStr = List.rev >> List.toArray >> System.String

let private processFinalState (tokens, state) =
    match state with
    | Initial _ -> tokens
    | ReadingStr (currentPos, start, cs) ->
        (Str(Range(start, currentPos), charsToStr cs)
         :: tokens)

let private folder (tokens, state) c =
    match state with
    | Initial currentPos ->
        match c with
        | '(' -> (LeftParen currentPos :: tokens, Initial(Pos.nextColumn currentPos))
        | ')' -> (RightParen currentPos :: tokens, Initial(Pos.nextColumn currentPos))
        | '[' -> (LeftBracket currentPos :: tokens, Initial(Pos.nextColumn currentPos))
        | ']' -> (RightBracket currentPos :: tokens, Initial(Pos.nextColumn currentPos))
        | '\n' -> (tokens, Initial(Pos.nextLine currentPos))
        | c when isWhiteSpace c -> (tokens, Initial(Pos.nextColumn currentPos))
        | c -> (tokens, ReadingStr(Pos.nextColumn currentPos, currentPos, [ c ]))
    | ReadingStr (currentPos, start, cs) ->
        match c with
        | '(' ->
            (LeftParen currentPos
             :: Str(Range(start, Pos.previousColumn currentPos), charsToStr cs)
                :: tokens,
             Initial(Pos.nextColumn currentPos))
        | ')' ->
            (RightParen currentPos
             :: Str(Range(start, Pos.previousColumn currentPos), charsToStr cs)
                :: tokens,
             Initial(Pos.nextColumn currentPos))
        | '[' ->
            (LeftBracket currentPos
             :: Str(Range(start, Pos.previousColumn currentPos), charsToStr cs)
                :: tokens,
             Initial(Pos.nextColumn currentPos))
        | ']' ->
            (RightBracket currentPos
             :: Str(Range(start, Pos.previousColumn currentPos), charsToStr cs)
                :: tokens,
             Initial(Pos.nextColumn currentPos))
        | '\n' ->
            (Str(Range(start, Pos.previousColumn currentPos), charsToStr cs)
             :: tokens,
             Initial(Pos.nextLine currentPos))
        | c when isWhiteSpace c ->
            (Str(Range(start, Pos.previousColumn currentPos), charsToStr cs)
             :: tokens,
             Initial(Pos.nextColumn currentPos))
        | c -> (tokens, ReadingStr(Pos.nextColumn currentPos, start, c :: cs))

let lex (src: string) =
    src
    |> Seq.fold folder ([], Initial(Pos(0u, 0u)))
    |> processFinalState
    |> List.rev

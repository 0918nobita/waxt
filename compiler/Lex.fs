module Waxt.Compiler.Lex

open Location
open Token

type private LexState =
    | Initial of currentPos: Point
    | ReadingStr of currentPos: Point * start: Point * list<char>

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
        | '(' -> (LeftParen currentPos :: tokens, Initial(Point.nextColumn currentPos))
        | ')' -> (RightParen currentPos :: tokens, Initial(Point.nextColumn currentPos))
        | '[' -> (LeftBracket currentPos :: tokens, Initial(Point.nextColumn currentPos))
        | ']' -> (RightBracket currentPos :: tokens, Initial(Point.nextColumn currentPos))
        | '\n' -> (tokens, Initial(Point.nextLine currentPos))
        | c when isWhiteSpace c -> (tokens, Initial(Point.nextColumn currentPos))
        | c -> (tokens, ReadingStr(Point.nextColumn currentPos, currentPos, [ c ]))
    | ReadingStr (currentPos, start, cs) ->
        match c with
        | '(' ->
            (LeftParen currentPos
             :: Str(Range(start, Point.previousColumn currentPos), charsToStr cs)
                :: tokens,
             Initial(Point.nextColumn currentPos))
        | ')' ->
            (RightParen currentPos
             :: Str(Range(start, Point.previousColumn currentPos), charsToStr cs)
                :: tokens,
             Initial(Point.nextColumn currentPos))
        | '[' ->
            (LeftBracket currentPos
             :: Str(Range(start, Point.previousColumn currentPos), charsToStr cs)
                :: tokens,
             Initial(Point.nextColumn currentPos))
        | ']' ->
            (RightBracket currentPos
             :: Str(Range(start, Point.previousColumn currentPos), charsToStr cs)
                :: tokens,
             Initial(Point.nextColumn currentPos))
        | '\n' ->
            (Str(Range(start, Point.previousColumn currentPos), charsToStr cs)
             :: tokens,
             Initial(Point.nextLine currentPos))
        | c when isWhiteSpace c ->
            (Str(Range(start, Point.previousColumn currentPos), charsToStr cs)
             :: tokens,
             Initial(Point.nextColumn currentPos))
        | c -> (tokens, ReadingStr(Point.nextColumn currentPos, start, c :: cs))

let lex (src: string) =
    src
    |> Seq.fold folder ([], Initial(Point(0u, 0u)))
    |> processFinalState
    |> List.rev

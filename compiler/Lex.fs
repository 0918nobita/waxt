module Waxt.Compiler.Lex

open Token

type private LexState =
    | Initial of currentPos: Pos
    | ReadingCR of currentPos: Pos
    | ReadingStr of currentPos: Pos * start: Pos * list<char>

let private isWhiteSpace = System.Char.IsWhiteSpace

let private charsToStr = List.rev >> List.toArray >> System.String

let private processFinalState (tokens, state) =
    match state with
    | Initial _ -> tokens
    | ReadingCR _ -> failwith "Unexpected carriage return"
    | ReadingStr (currentPos, start, cs) ->
        (Str(charsToStr cs, Range(start, currentPos))
         :: tokens)

let (|CLeftParen|_|) (c, pos) =
    match c with
    | '(' -> Some(LeftParen pos)
    | _ -> None

let (|CRightParen|_|) (c, pos) =
    match c with
    | ')' -> Some(RightParen pos)
    | _ -> None

let (|CLeftBracket|_|) (c, pos) =
    match c with
    | '[' -> Some(LeftBracket pos)
    | _ -> None

let (|CRightBracket|_|) (c, pos) =
    match c with
    | ']' -> Some(RightBracket pos)
    | _ -> None

let private folder (tokens, state) c =
    match state with
    | Initial currentPos ->
        match c, currentPos with
        | CLeftParen tok
        | CRightParen tok
        | CLeftBracket tok
        | CRightBracket tok -> (tok :: tokens, Initial(Pos.nextColumn currentPos))
        | '\n', _ -> (tokens, Initial(Pos.nextLine currentPos))
        | '\r', _ -> (tokens, ReadingCR(Pos.nextColumn currentPos))
        | c, _ when isWhiteSpace c -> (tokens, Initial(Pos.nextColumn currentPos))
        | c, _ -> (tokens, ReadingStr(Pos.nextColumn currentPos, currentPos, [ c ]))

    | ReadingCR currentPos ->
        match c with
        | '\n' -> (tokens, Initial(Pos.nextLine currentPos))
        | _ -> failwith $"Line feed expected, but not found"

    | ReadingStr (currentPos, start, cs) ->
        match c, currentPos with
        | CLeftParen tok
        | CRightParen tok
        | CLeftBracket tok
        | CRightBracket tok ->
            (tok
             :: Str(charsToStr cs, Range(start, Pos.previousColumn currentPos))
                :: tokens,
             Initial(Pos.nextColumn currentPos))
        | '\n', _ ->
            (Str(charsToStr cs, Range(start, Pos.previousColumn currentPos))
             :: tokens,
             Initial(Pos.nextLine currentPos))
        | '\r', _ ->
            (Str(charsToStr cs, Range(start, Pos.previousColumn currentPos))
             :: tokens,
             ReadingCR(Pos.nextColumn currentPos))
        | c, _ when isWhiteSpace c ->
            (Str(charsToStr cs, Range(start, Pos.previousColumn currentPos))
             :: tokens,
             Initial(Pos.nextColumn currentPos))
        | c, _ -> (tokens, ReadingStr(Pos.nextColumn currentPos, start, c :: cs))

let lex (src: string) =
    src
    |> Seq.fold folder ([], Initial(Pos(0u, 0u)))
    |> processFinalState
    |> List.rev

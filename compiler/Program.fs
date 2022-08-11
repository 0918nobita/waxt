printfn "WAXT Compiler"

let input = "(+ 1 2)"

type Token =
    | LeftParen
    | RightParen
    | LeftSquareBracket
    | RightSquareBracket
    | Str of string

type LexState =
    | Initial
    | ReadingStr of list<char>

let charListToStr = List.toArray >> System.String

let isWhiteSpace = System.Char.IsWhiteSpace

let lex (src: string) =
    let folder (tokens, state) c =
        match state with
        | Initial ->
            match c with
            | '(' -> (LeftParen :: tokens, Initial)
            | ')' -> (RightParen :: tokens, Initial)
            | '[' -> (LeftSquareBracket :: tokens, Initial)
            | ']' -> (RightSquareBracket :: tokens, Initial)
            | c when isWhiteSpace c -> (tokens, Initial)
            | c -> (tokens, ReadingStr [ c ])
        | ReadingStr cs ->
            match c with
            | '(' -> (LeftParen :: Str(charListToStr cs) :: tokens, Initial)
            | ')' -> (RightParen :: Str(charListToStr cs) :: tokens, Initial)
            | '[' ->
                (LeftSquareBracket
                 :: Str(charListToStr cs) :: tokens,
                 Initial)
            | ']' ->
                (RightSquareBracket
                 :: Str(charListToStr cs) :: tokens,
                 Initial)
            | c when isWhiteSpace c -> (Str(charListToStr cs) :: tokens, Initial)
            | c -> (tokens, ReadingStr(c :: cs))

    let processFinalState (tokens, state) =
        match state with
        | Initial -> tokens
        | ReadingStr cs -> (Str(charListToStr cs) :: tokens)

    src
    |> Seq.fold folder ([], Initial)
    |> processFinalState
    |> List.rev

printfn "%A" <| lex input

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

let charListToString = List.toArray >> System.String

let isWhiteSpace = System.Char.IsWhiteSpace

let lex (src: string) =
    src + " "
    |> Seq.fold
        (fun (tokens, state) c ->
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
                | '(' -> (LeftParen :: Str(charListToString cs) :: tokens, Initial)
                | ')' -> (RightParen :: Str(charListToString cs) :: tokens, Initial)
                | '[' ->
                    (LeftSquareBracket
                     :: Str(charListToString cs) :: tokens,
                     Initial)
                | ']' ->
                    (RightSquareBracket
                     :: Str(charListToString cs) :: tokens,
                     Initial)
                | c when isWhiteSpace c -> (Str(charListToString cs) :: tokens, Initial)
                | c -> (tokens, ReadingStr(c :: cs)))
        ([], Initial)
    |> fst
    |> List.rev

printfn "%A" <| lex input

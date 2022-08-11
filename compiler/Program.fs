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

type SExpr =
    | Atom of string
    | List of list<SExpr>

module SExpr =
    let rec toString =
        function
        | Atom str -> str
        | List list ->
            list
            |> List.map toString
            |> String.concat " "
            |> sprintf "(%s)"

open FsToolkit.ErrorHandling

let rec parse (tokens: list<Token>) : Result<SExpr * list<Token>, string> =
    match tokens with
    | [] -> Error "Unexpected end of input"
    | (Str str) :: rest -> Ok(Atom str, rest)
    | LeftParen :: rest ->
        result {
            let! (exprs, rest) = parseMany rest

            return!
                match rest with
                | RightParen :: rest -> Ok(List exprs, rest)
                | _ -> Error "Expected ')'"
        }
    | RightParen :: _ -> Error "Unexpected ')'"
    | _ -> failwith "not implemented"

and parseMany (tokens: list<Token>) : Result<list<SExpr> * list<Token>, string> =
    match tokens with
    | [] -> Ok([], [])
    | _ ->
        match parse tokens with
        | Error _ -> Ok([], tokens)
        | Ok (expr, rest) ->
            result {
                let! (exprs, rest) = parseMany rest
                return (expr :: exprs, rest)
            }

let input = "(* (+ 1 2) 4)"
input |> lex |> parse |> printfn "%A"

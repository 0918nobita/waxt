type Token =
    | LeftParen
    | RightParen
    | LeftBracket
    | RightBracket
    | Str of string

type LexState =
    | Initial
    | ReadingStr of list<char>

let lex (src: string) =
    let charsToStr = List.rev >> List.toArray >> System.String

    let folder (tokens, state) c =
        let isWhiteSpace = System.Char.IsWhiteSpace

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

    let processFinalState (tokens, state) =
        match state with
        | Initial -> tokens
        | ReadingStr cs -> (Str(charsToStr cs) :: tokens)

    src
    |> Seq.fold folder ([], Initial)
    |> processFinalState
    |> List.rev

type SExpr =
    | Atom of string
    | ParenList of list<SExpr>
    | BracketList of list<SExpr>

module SExpr =
    let rec toString =
        function
        | Atom str -> str
        | ParenList list ->
            list
            |> List.map toString
            |> String.concat " "
            |> sprintf "(%s)"
        | BracketList list ->
            list
            |> List.map toString
            |> String.concat " "
            |> sprintf "[%s]"

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
                | RightParen :: rest -> Ok(ParenList exprs, rest)
                | _ -> Error "Expected ')'"
        }
    | RightParen :: _ -> Error "Unexpected ')'"
    | LeftBracket :: rest ->
        result {
            let! (exprs, rest) = parseMany rest

            return!
                match rest with
                | RightBracket :: rest -> Ok(BracketList exprs, rest)
                | _ -> Error "Expected ']'"
        }
    | RightBracket :: _ -> Error "Unexpected ']'"

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

let input =
    "(func add-and-store [addr i32 x i32 y i32] (i32.store addr (i32.add x y)))"

input |> lex |> parse |> printfn "%A"

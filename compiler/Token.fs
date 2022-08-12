module Waxt.Compiler.Token

open Location

type Token =
    | LeftParen of Pos
    | RightParen of Pos
    | LeftBracket of Pos
    | RightBracket of Pos
    | Str of Range * string

    interface ILocatable with
        member this.Locate() =
            match this with
            | LeftParen pos
            | RightParen pos
            | LeftBracket pos
            | RightBracket pos -> Range.fromPos pos
            | Str (range, _) -> range

module Token =
    let toString =
        function
        | LeftParen _ -> "("
        | RightParen _ -> ")"
        | LeftBracket _ -> "["
        | RightBracket _ -> "]"
        | Str (_, str) -> str

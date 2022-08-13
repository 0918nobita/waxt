module Waxt.Compiler.Token

type Token =
    | LeftParen of Pos
    | RightParen of Pos
    | LeftBracket of Pos
    | RightBracket of Pos
    | Str of raw: string * at: Range

    interface ILocatable with
        member this.Locate() =
            match this with
            | LeftParen pos
            | RightParen pos
            | LeftBracket pos
            | RightBracket pos -> Range.fromPos pos
            | Str (_, at) -> at

module Token =
    let toString =
        function
        | LeftParen _ -> "("
        | RightParen _ -> ")"
        | LeftBracket _ -> "["
        | RightBracket _ -> "]"
        | Str (str, _) -> str

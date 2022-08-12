module Waxt.Compiler.Token

open Location

type Token =
    | LeftParen of Point
    | RightParen of Point
    | LeftBracket of Point
    | RightBracket of Point
    | Str of Range * string

    interface ILocatable with
        member this.Locate() =
            match this with
            | LeftParen point
            | RightParen point
            | LeftBracket point
            | RightBracket point -> Range.fromPoint point
            | Str (range, _) -> range

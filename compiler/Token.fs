module Waxt.Compiler.Token

type Token =
    | LeftParen
    | RightParen
    | LeftBracket
    | RightBracket
    | Str of string

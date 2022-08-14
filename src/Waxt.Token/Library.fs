[<AutoOpen>]
module Waxt.Token.Library

open Waxt.Location

/// 構文の最小単位。字句解析器の出力・構文解析器の入力で用いられる。
type Token =
    /// `(`
    | LeftParen of Pos
    /// `)
    | RightParen of Pos
    /// `[`
    | LeftBracket of Pos
    /// `]`
    | RightBracket of Pos
    /// 括弧または空白文字で区切って得られる、連続した文字列
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

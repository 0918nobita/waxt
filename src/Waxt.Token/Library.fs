namespace Waxt.Token

open Thoth.Json.Net
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

    let toJson =
        function
        | LeftParen pos ->
            Encode.object [ "token", Encode.string "("
                            "at", Encode.string (Pos.toString pos) ]

        | RightParen pos ->
            Encode.object [ "token", Encode.string ")"
                            "at", Encode.string (Pos.toString pos) ]

        | LeftBracket pos ->
            Encode.object [ "token", Encode.string "["
                            "at", Encode.string (Pos.toString pos) ]

        | RightBracket pos ->
            Encode.object [ "token", Encode.string "]"
                            "at", Encode.string (Pos.toString pos) ]

        | Str (str, at) ->
            Encode.object [ "token", Encode.string str
                            "at", Encode.string (Range.toString at) ]

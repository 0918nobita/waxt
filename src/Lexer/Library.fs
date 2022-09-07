module Waxt.Lexer

open Waxt.Location

/// 字句解析中に発生するエラー
type LexError =
    private
    /// キャリッジリターンの直後にラインフィードが続いていない
    | LineFeedNotFound of Pos

    override this.ToString() =
        match this with
        | LineFeedNotFound pos ->
            let pos = string pos
            $"(%s{pos}) Line feed not found"

[<AutoOpen>]
module Waxt.Parser.SExpr

open Waxt.Location

/// S式
type SExpr =
    /// S式の構文の最小単位である文字列
    | Atom of content: string * at: Range
    /// `(`, `)` 括弧で囲われたリスト
    | ParenList of members: list<SExpr> * at: Range
    /// `[`, `]` 括弧で囲われたリスト
    | BracketList of members: list<SExpr> * at: Range

    interface ILocatable with
        member this.Locate() =
            match this with
            | Atom (_, at)
            | ParenList (_, at)
            | BracketList (_, at) -> at

module SExpr =
    let rec toString =
        function
        | Atom (content, _) -> content

        | ParenList (list, _) ->
            list
            |> List.map toString
            |> String.concat " "
            |> sprintf "(%s)"

        | BracketList (list, _) ->
            list
            |> List.map toString
            |> String.concat " "
            |> sprintf "[%s]"

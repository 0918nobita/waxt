[<AutoOpen>]
module Waxt.Compiler.SExpr

type SExpr =
    | Atom of content: string * at: Range
    | ParenList of members: list<SExpr> * at: Range
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
        | Atom (str, _) -> str

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

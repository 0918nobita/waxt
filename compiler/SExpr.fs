module Waxt.Compiler.SExpr

open Location

type SExpr =
    | Atom of range: Range * content: string
    | ParenList of range: Range * members: list<SExpr>
    | BracketList of range: Range * members: list<SExpr>

module SExpr =
    let rec toString =
        function
        | Atom (_, str) -> str

        | ParenList (_, list) ->
            list
            |> List.map toString
            |> String.concat " "
            |> sprintf "(%s)"

        | BracketList (_, list) ->
            list
            |> List.map toString
            |> String.concat " "
            |> sprintf "[%s]"

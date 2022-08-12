module Waxt.Compiler.SExpr

type SExpr =
    | Atom of string
    | ParenList of list<SExpr>
    | BracketList of list<SExpr>

module SExpr =
    let rec toString =
        function
        | Atom str -> str

        | ParenList list ->
            list
            |> List.map toString
            |> String.concat " "
            |> sprintf "(%s)"

        | BracketList list ->
            list
            |> List.map toString
            |> String.concat " "
            |> sprintf "[%s]"

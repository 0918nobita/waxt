module Waxt.Compiler.Program

open FsToolkit.ErrorHandling
open Location

let compile src =
    let tokens = Lex.lex src

    result {
        let! (sExpr, _rest) = ParseSExpr.parseSExpr (Point(0u, 0u)) tokens

        return! ParseStmt.parseStmt sExpr
    }

let input =
    "(func add-and-store [addr i32 x i32 y i32] (i32.store addr (i32.add x y)))"

input |> compile |> printfn "%A"

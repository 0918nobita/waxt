module Waxt.Compiler.Program

let input =
    "(func add-and-store [addr i32 x i32 y i32] (i32.store addr (i32.add x y)))"

input
|> Lex.lex
|> Parse.parseSExpr
|> printfn "%A"

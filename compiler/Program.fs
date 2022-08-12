module Waxt.Compiler.Program

type CompileError =
    | SExprParserError of ParseSExpr.ParseError
    | StmtParserError of ParseStmt.ParseError

open FsToolkit.ErrorHandling

let compile src =
    let tokens = Lex.lex src

    result {
        let! (sExpr, _rest) =
            tokens
            |> ParseSExpr.parseSExpr
            |> Result.mapError SExprParserError

        return!
            ParseStmt.parseStmt sExpr
            |> Result.mapError StmtParserError
    }

let input =
    "(func add-and-store [addr i32 x i32 y i32] (i32.store addr (i32.add x y)))"

input |> compile |> printfn "%A"

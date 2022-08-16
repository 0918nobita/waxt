module Waxt.Parser.Test.Program

open Expecto
open Thoth.Json.Net
open VerifyExpecto
open Waxt.Lexer
open Waxt.Location
open Waxt.Parser
open Waxt.UntypedAst

let parsingToSExprShouldSucceed (src: string) =
    let (sExpr, rest) =
        Expect.wantOk (lex src) "lexical analysis should succeed"
        |> ParseSExpr.parseSExpr Pos.origin
        |> (fun parseResult -> Expect.wantOk parseResult "Parsing to S-expression should succeed")

    Expect.isEmpty rest "Parser should consume all tokens"
    sExpr

let parsingToStmtShouldSucceed (sExpr: SExpr) =
    sExpr
    |> ParseUntypedAst.parseStmt
    |> (fun parseResult -> Expect.wantOk parseResult "Parsing to statement should succeed")
    |> Stmt.toJson
    |> Encode.toString 2

[<Tests>]
let returnI32 =
    testTask "returnI32" {
        let sExpr =
            parsingToSExprShouldSucceed "(func foo i32 [x i32 y i32] (i32.mul (i32.add 1 2) 4))"

        do! Verifier.Verify("returnI32SExpr", SExpr.toString sExpr)

        let stmt = parsingToStmtShouldSucceed sExpr

        do! Verifier.Verify("returnI32", stmt)
    }

[<Tests>]
let voidFunc =
    testTask "voidFunc" {
        let sExpr =
            parsingToSExprShouldSucceed "(func add-and-store [addr i32 x i32 y i32] (i32.store addr (i32.add x y)))"

        do! Verifier.Verify("voidFuncSExpr", SExpr.toString sExpr)

        let stmt = parsingToStmtShouldSucceed sExpr

        do! Verifier.Verify("voidFunc", stmt)
    }

[<EntryPoint>]
let main argv = runTestsInAssembly defaultConfig argv

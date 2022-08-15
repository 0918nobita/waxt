module Waxt.Parser.Test.Program

open Expecto
open Thoth.Json.Net
open VerifyExpecto
open Waxt.Lexer
open Waxt.Location
open Waxt.Parser
open Waxt.UntypedAst

[<Tests>]
let parserTest =
    testTask "parseSExpr" {
        let (sExpr, rest) =
            Expect.wantOk
                (lex "(func foo i32 [x i32 y i32] (i32.mul (i32.add 1 2) 4))")
                "lexical analysis should succeed"
            |> ParseSExpr.parseSExpr Pos.origin
            |> (fun parseResult -> Expect.wantOk parseResult "Parsing to S-expression should succeed")

        Expect.isEmpty rest "Parser should consume all tokens"

        do! Verifier.Verify("parseSExpr", SExpr.toString sExpr)

        let result =
            ParseUntypedAst.parseStmt sExpr
            |> (fun parseResult -> Expect.wantOk parseResult "Parsing to Stmt should succeed")
            |> Stmt.toJson
            |> Encode.toString 2

        do! Verifier.Verify("parseStmt", result)
    }

[<EntryPoint>]
let main argv = runTestsInAssembly defaultConfig argv

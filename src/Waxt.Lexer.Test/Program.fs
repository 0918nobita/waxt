module Waxt.Lexer.Test.Program

open Expecto
open Thoth.Json.Net
open VerifyExpecto
open Waxt.Lexer
open Waxt.Token

[<Tests>]
let lexerTest =
    testTask "lex" {
        let result =
            Expect.wantOk (lex "(+ 1 2)") "lexical analysis should succeed"
            |> List.map Token.toJson
            |> Encode.list
            |> Encode.toString 2

        do! Verifier.Verify("lex", result)
    }

[<EntryPoint>]
let main argv = runTestsInAssembly defaultConfig argv

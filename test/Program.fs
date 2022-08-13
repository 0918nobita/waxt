module Waxt.Compiler.Test.Program

open Expecto
open Thoth.Json.Net
open VerifyExpecto
open Waxt.Compiler

let compilationShouldSucceed src =
    src
    |> compile
    |> (fun result -> Expect.wantOk result "The compilation should succeed")
    |> Stmt.toJson
    |> Encode.toString 2

[<Tests>]
let funcDefTest =
    testTask "funcDef" {
        let result =
            "(func foo i32 [x i32 y i32] (i32.mul (i32.add 1 2) 4))"
            |> compilationShouldSucceed

        do! Verifier.Verify("returnI32", result)

        let result =
            "(func add-and-store [addr i32 x i32 y i32] (i32.store addr (i32.add x y)))"
            |> compilationShouldSucceed

        do! Verifier.Verify("voidFunc", result)
    }

[<EntryPoint>]
let main argv = runTestsInAssembly defaultConfig argv

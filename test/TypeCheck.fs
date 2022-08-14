module Waxt.Compiler.Test.TypeCheck

open Expecto
open Thoth.Json.Net
open VerifyExpecto
open Waxt.Compiler

let typeCheckShouldSucceed (typeEnv: TypeEnv.T) (expr: Expr.T) =
    expr
    |> checkType typeEnv
    |> (fun result -> Expect.wantOk result "The type check should succeed")
    |> TypedExpr.toJson
    |> Encode.toString 2

let private range = Range(Pos(0u, 0u), Pos(0u, 0u))

[<Tests>]
let i32ConstTest =
    testTask "i32Const" {
        let result =
            Expr.I32Const(6, range)
            |> typeCheckShouldSucceed TypeEnv.empty

        do! Verifier.Verify("i32Const", result)
    }

[<Tests>]
let undefinedVarTest =
    testTask "undefinedVar" {
        let result =
            Expr.Var("x", range)
            |> checkType TypeEnv.empty
            |> (fun result -> Expect.wantError result "The type check should fail")

        do! Verifier.Verify("undefinedVar", result)
    }

[<Tests>]
let varTest =
    testTask "var" {
        let result =
            Expr.Var("x", range)
            |> typeCheckShouldSucceed (TypeEnv.empty |> TypeEnv.add "x" Type.I32)

        do! Verifier.Verify("var", result)
    }

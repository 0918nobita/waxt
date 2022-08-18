module Waxt.TypeChecker.Test.Program

open Expecto
open Thoth.Json.Net
open VerifyExpecto
open Waxt.Location
open Waxt.Type
open Waxt.TypeChecker
open Waxt.TypedAst
open Waxt.UntypedAst

[<Tests>]
let getFuncSigsTest =
    test "getFuncSigs" {
        let range = Range.fromPos Pos.origin
        let funcDef =
            FuncDef(FuncName("foo", range), I32 None, [], [ I32Const(12, range) ])

        let untypedFuncs =
            Expect.wantOk (getFuncSigs [ funcDef ]) "compile untyped statements to function signatures"

        Expect.wantError (getFuncSigs [ funcDef; funcDef ]) "duplicate function definition"
        |> ignore

        typeFuncBodies untypedFuncs |> ignore
    }

let typeCheckShouldSucceed (typeEnv: TypeEnv) (expr: Expr) =
    expr
    |> checkType typeEnv
    |> (fun result -> Expect.wantOk result "The type check should succeed")
    |> TypedExpr.toJson
    |> Encode.toString 2

let private range = Range.fromPos Pos.origin

[<Tests>]
let i32ConstTest =
    testTask "i32Const" {
        let result =
            I32Const(6, range)
            |> typeCheckShouldSucceed TypeEnv.empty

        do! Verifier.Verify("i32Const", result)
    }

[<Tests>]
let undefinedVarTest =
    testTask "undefinedVar" {
        let errorMsg =
            Var("x", range)
            |> checkType TypeEnv.empty
            |> (fun result -> Expect.wantError result "The type check should fail")
            |> TypeError.toString

        do! Verifier.Verify("undefinedVar", errorMsg)
    }

[<Tests>]
let varTest =
    testTask "var" {
        let result =
            Var("x", range)
            |> typeCheckShouldSucceed (TypeEnv.empty |> TypeEnv.add "x" (I32 None))

        do! Verifier.Verify("var", result)
    }

[<EntryPoint>]
let main argv = runTestsInAssembly defaultConfig argv
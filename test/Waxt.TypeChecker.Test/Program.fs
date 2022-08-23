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
let checkFuncSigsTest =
    test "checkFuncSigs" {
        let range = Range.fromPos Pos.origin
        let funcDef = FuncDef(FuncName("foo", range), I32, [], [ I32Const(12, range) ])

        Expect.wantOk (typeFuncDefs [ funcDef ]) "compile untyped statements to function signatures"
        |> ignore

        Expect.wantError (typeFuncDefs [ funcDef; funcDef ]) "duplicate function definition"
        |> ignore
    }

let typeCheckShouldSucceed (typeEnv: TypeEnv) (expr: Expr) =
    expr
    |> typeExpr typeEnv
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
            |> typeExpr TypeEnv.empty
            |> (fun result -> Expect.wantError result "The type check should fail")
            |> TypeError.toString

        do! Verifier.Verify("undefinedVar", errorMsg)
    }

[<Tests>]
let varTest =
    testTask "var" {
        let result =
            Var("x", range)
            |> typeCheckShouldSucceed (
                TypeEnv.empty
                |> TypeEnv.add ("x", Range.fromPos Pos.origin) (I32, Range.fromPos Pos.origin)
            )

        do! Verifier.Verify("var", result)
    }

[<EntryPoint>]
let main argv = runTestsInAssembly defaultConfig argv

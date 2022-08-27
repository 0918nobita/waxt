module Wasm

open NUnit.Framework
open Waxt.TypeInferrer.Extract
open Waxt.TypeInferrer.FuncContext
open Waxt.TypeInferrer.Term
open Waxt.TypeInferrer.Type
open Waxt.TypeInferrer.TypeLiteral
open Waxt.TypeInferrer.Unify
open Waxt.TypeInferrer.VarContext

[<SetUp>]
let Setup () = ()

let wantOk (res: Result<'a, 'b>) : 'a =
    match res with
    | Ok value -> value
    | Error _ ->
        Assert.Fail "Expected Ok"
        failwith "unreachable"

[<Test>]
let Test1 () =
    let (simulEquation, ty) =
        let funcContext =
            FuncContext.empty
            |> FuncContext.add (FuncName.make "fact") (FuncType([ TyVar "t0" ], TyVar "t1"))

        let varContext =
            VarContext.empty
            |> VarContext.add (VarName.make "n") (TyVar "t0")

        extract
            funcContext
            varContext
            (If(
                I32Eqz(Var(VarName.make "n")),
                I32Const 1,
                I32Mul(
                    Var(VarName.make "n"),
                    Application(FuncName.make "fact", [ I32Sub(Var(VarName.make "n"), I32Const 1) ])
                )
            ))
        |> wantOk

    printfn "%O" simulEquation
    printfn "%O" ty

    unify simulEquation |> wantOk |> printfn "%A"

    Assert.Pass()

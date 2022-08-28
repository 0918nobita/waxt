module Wasm

open NUnit.Framework
open Waxt.TypeInferrer.Extract
open Waxt.TypeInferrer.FuncContext
open Waxt.TypeInferrer.Term
open Waxt.TypeInferrer.Type
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
        let fact = FuncName.make "fact"
        let n = VarName.make "n"

        let t0 = TyVar(TyVarName.make "t0")
        let t1 = TyVar(TyVarName.make "t1")

        let funcContext =
            FuncContext.empty
            |> FuncContext.add fact (FuncType([ t0 ], t1))

        let varContext = VarContext.empty |> VarContext.add n t0

        extract
            funcContext
            varContext
            (If(I32Eqz(Var n), I32Const 1, I32Mul(Var n, Application(fact, [ I32Sub(Var n, I32Const 1) ]))))
        |> wantOk

    printfn "%O" simulEquation
    printfn "%O" ty

    unify simulEquation |> wantOk |> printfn "%A"

    Assert.Pass()

module Wasm

open NUnit.Framework
open Waxt.TypeInferrer.Extract
open Waxt.TypeInferrer.FuncContext
open Waxt.TypeInferrer.Term
open Waxt.TypeInferrer.TypeLiteral
open Waxt.TypeInferrer.VarContext

[<SetUp>]
let Setup () = ()

[<Test>]
let Test1 () =
    extract
        FuncContext.empty
        VarContext.empty
        (LetWithType(VarName.make "x", I32TyLit, I32Const 3, I32Add(Var(VarName.make "x"), I32Const 4)))
    |> printfn "%A"

    Assert.Pass()

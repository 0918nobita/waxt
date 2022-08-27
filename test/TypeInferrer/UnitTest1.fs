module Wasm

open FsUnit
open NUnit.Framework
open Waxt.TypeInferrer

[<SetUp>]
let Setup () = ()

[<Test>]
let Test1 () =
    extract Context.empty (LetWithType("x", I32TyLit, I32Const 3, I32Add(Var "x", I32Const 4)))
    |> printfn "%A"

    Assert.Pass()

module Wasm

open FsUnit
open NUnit.Framework
open Waxt.TypeInferrer

[<SetUp>]
let Setup () = ()

[<Test>]
let Test1 () = Assert.Pass()

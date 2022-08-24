module Wasm

open NUnit.Framework
open Waxt.Wasm

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    Assert.AreEqual(foo 4, 7)

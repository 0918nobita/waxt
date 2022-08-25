module Wasm

open FsUnit
open NUnit.Framework
open Waxt.Wasm

[<SetUp>]
let Setup () = ()

[<Test>]
let Test1 () = (foo 4) |> should equal 7

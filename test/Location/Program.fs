module Waxt.Location.Test.Program

open NUnit.Framework
open Waxt.Location

[<Test>]
let Test1 () =
    let start = Pos.make 1 0
    Assert.AreEqual(string start, "2:1")
    let end_ = Pos.make 2 3
    Assert.AreEqual(string end_, "3:4")
    let range = Range.make start end_
    Assert.AreEqual(string range, "2:1-3:4")

[<EntryPoint>]
let main _ = 0

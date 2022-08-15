module Waxt.Location.Test.Program

open Expecto
open Waxt.Location

[<Tests>]
let nextColTest =
    test "nextCol" {
        let actual = Pos.nextCol (Pos.make 1u 1u)
        let expected = Pos.make 1u 2u
        Expect.equal actual expected "column should be incremented"
    }

[<Tests>]
let nextLineTest =
    test "nextLine" {
        let actual = Pos.nextLine (Pos.make 2u 3u)
        let expected = Pos.make 3u 0u
        Expect.equal actual expected "column should be 0"
    }

[<EntryPoint>]
let main argv = runTestsInAssembly defaultConfig argv

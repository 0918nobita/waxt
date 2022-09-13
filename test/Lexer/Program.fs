module Waxt.Lexer.Test.Program

open NUnit.Framework
open Thoth.Json.Net
open Waxt.Lexer
open Waxt.TestUtil
open Waxt.Token

[<Test>]
let Test1 () =
    let tokens = lex (LexOption Lf) "(foo bar baz)"
    SnapshotTest.VerifyJSON(tokens |> List.map Token.toJSON |> Encode.list)

[<EntryPoint>]
let main _ = 0

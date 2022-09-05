module Waxt.TestUtil

open NUnit.Framework
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Thoth.Json.Net
open VerifyTests
open VerifyNUnit

let verifySettings = VerifySettings()
verifySettings.UseExtension("json")

let wantOk (res: Result<'a, 'b>) : 'a =
    match res with
    | Ok value -> value
    | Error _ ->
        Assert.Fail "Expected Ok"
        failwith "unreachable"

type SnapshotTest =
    static member VerifyJSON(target: JsonValue, [<CallerFilePath; Optional; DefaultParameterValue("")>] sourceFile) =
        task {
            let! _ = Verifier.Verify(Encode.toString 2 target, verifySettings, sourceFile)
            return ()
        }

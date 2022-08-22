[<AutoOpen>]
module Waxt.TypeChecker.Expect

open Waxt.Location
open Waxt.Type

let expectType (expected: Type) ((actual, range): Type * Range) =
    if Type.equal expected actual then
        Ok()
    else
        let expected = Type.toString expected
        let actual = Type.toString actual
        Error(TypeError($"Type mismatch, expected: `%s{expected}`, actual: `%s{actual}`", range))

[<AutoOpen>]
module Waxt.TypeChecker.Expect

open Waxt.Type

let expectType (expected: Type) (actual: Type) =
    if Type.equal expected actual then
        Ok()
    else
        let expected = Type.toString expected
        let range = Type.tryGetRange actual
        let actual = Type.toString actual
        Error(TypeError($"Type mismatch, expected: `%s{expected}`, actual: `%s{actual}`", range))

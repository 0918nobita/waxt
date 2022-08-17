module Waxt.TypeChecker.Test.IndexedMap

open Expecto
open Waxt.TypeChecker

[<Tests>]
let indexedMapTest =
    test "indexedMap" {
        let indexedMap = IndexedMap<string, string>.Empty
        Expect.equal (indexedMap.TryItem "foo") None "IndexedMap should be empty"

        indexedMap.Add("foo", "fooValue")
        Expect.equal (indexedMap.TryItem "foo") (Some "fooValue") "IndexedMap should contain foo"
        Expect.equal (indexedMap.TryItemByIndex 0) (Some "fooValue") "foo should be specified by index"

        indexedMap.Add("bar", "barValue")
        Expect.equal (indexedMap.TryItem "bar") (Some "barValue") "IndexedMap should contain bar"
        Expect.equal (indexedMap.TryItemByIndex 0) (Some "fooValue") "foo should be specified by index 0"
        Expect.equal (indexedMap.TryItemByIndex 1) (Some "barValue") "bar should be specified by index 1"
    }

module Waxt.TypeChecker.Test.IndexedMap

open Expecto
open Waxt.TypeChecker

[<Tests>]
let indexedMapTest =
    test "indexedMap" {
        let indexedMap = IndexedMap<string, string>(2)
        Expect.equal indexedMap["foo"] None "IndexedMap should be empty"
        Expect.equal indexedMap.Count 0 "Count of IndexedMap should be incremented"

        indexedMap.Add("foo", "fooValue")
        Expect.equal indexedMap["foo"] (Some "fooValue") "IndexedMap should contain foo"
        Expect.equal (indexedMap.TryNth 0) (Some "fooValue") "foo should be specified by index"
        Expect.equal indexedMap.Count 1 "Count of IndexedMap should be incremented"

        indexedMap.Add("bar", "barValue")
        Expect.equal indexedMap["bar"] (Some "barValue") "IndexedMap should contain bar"
        Expect.equal (indexedMap.TryNth 0) (Some "fooValue") "foo should be specified by index 0"
        Expect.equal (indexedMap.TryNth 1) (Some "barValue") "bar should be specified by index 1"
        Expect.equal indexedMap.Count 2 "Count of IndexedMap should be incremented"

        let actual = indexedMap |> Seq.toList

        let expected =
            [ ("foo", "fooValue")
              ("bar", "barValue") ]

        Expect.equal actual expected "Convert IndexedMap to (key, value) list"
    }

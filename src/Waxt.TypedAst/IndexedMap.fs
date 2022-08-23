[<AutoOpen>]
module Waxt.TypedAst.IndexedMap

open System
open System.Collections.Generic

type IndexedMapEnumerator<'K, 'V>
    (
        valuesEnumerator: IEnumerator<'V>,
        mappingEnumerator: IEnumerator<KeyValuePair<'K, int>>
    ) =
    member _.current
        with private get () = (mappingEnumerator.Current.Key, valuesEnumerator.Current)

    interface IDisposable with
        member _.Dispose() =
            valuesEnumerator.Dispose()
            mappingEnumerator.Dispose()

    interface IEnumerator<'K * 'V> with
        member this.Current = this.current

    interface Collections.IEnumerator with
        member this.Current = this.current

        member _.MoveNext() =
            let k = mappingEnumerator.MoveNext()
            let v = valuesEnumerator.MoveNext()

            if k <> v then
                failwith "Fatal error: illegal state in IndexedMapEnumerator"

            k

        member _.Reset() =
            mappingEnumerator.Reset()
            valuesEnumerator.Reset()

/// キーと整数インデックスの両方で要素にアクセス可能なマップ
type IndexedMap<'K, 'V when 'K: comparison> private (values: ResizeArray<'V>, mapping: Dictionary<'K, int>) =
    new(capacity: int) = IndexedMap<'K, 'V>(ResizeArray(capacity), new Dictionary<'K, int>())

    member _.Count = values.Count

    member _.Add(key: 'K, value: 'V) =
        mapping.Add(key, values.Count)
        values.Add(value)

    member _.Item
        with get (key: 'K): option<'V> =
            try
                Some values[mapping[key]]
            with
            | :? KeyNotFoundException -> None
            | :? ArgumentOutOfRangeException -> failwith "Fatal error: illegal state in IndexedMap"

    member _.TryNth(index: int) : option<'V> =
        try
            Some values[index]
        with
        | :? ArgumentOutOfRangeException -> None

    member _.Exists(key: 'K) : bool = mapping.ContainsKey key

    member _.DebugPrint() =
        for keyValuePair in mapping do
            printfn "%A: %A" keyValuePair.Key values[keyValuePair.Value]

    member private _.getEnumerator() =
        new IndexedMapEnumerator<'K, 'V>(values.GetEnumerator(), mapping.GetEnumerator())

    interface Collections.IEnumerable with
        member this.GetEnumerator() = this.getEnumerator ()

    interface IEnumerable<'K * 'V> with
        member this.GetEnumerator() = this.getEnumerator ()

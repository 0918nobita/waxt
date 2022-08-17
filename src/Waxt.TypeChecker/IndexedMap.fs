[<AutoOpen>]
module Waxt.TypeChecker.IndexedMap

open System
open System.Collections.Generic

type IndexedMapEnumerator<'K, 'V>
    (
        valuesEnumerator: IEnumerator<'V>,
        mappingEnumerator: IEnumerator<KeyValuePair<'K, int>>
    ) =
    member _.current_
        with private get () = (mappingEnumerator.Current.Key, valuesEnumerator.Current)

    interface IDisposable with
        member _.Dispose() =
            valuesEnumerator.Dispose()
            mappingEnumerator.Dispose()

    interface IEnumerator<'K * 'V> with
        member this.Current = this.current_

    interface Collections.IEnumerator with
        member this.Current = this.current_ :> obj

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
type IndexedMap<'K, 'V when 'K: comparison> private (values: seq<'V>, mapping: seq<'K * int>) =
    let keyIndexDict = new Dictionary<'K, int>()

    do mapping |> Seq.iter keyIndexDict.Add

    member val private Values = ResizeArray values

    member val private Mapping = keyIndexDict

    member this.Add(key: 'K, value: 'V) =
        this.Mapping.Add(key, this.Values.Count)
        this.Values.Add(value)

    member this.Item
        with get (key: 'K): option<'V> =
            try
                Some this.Values[this.Mapping[key]]
            with
            | :? KeyNotFoundException -> None
            | :? ArgumentOutOfRangeException -> failwith "Fatal error: illegal state in IndexedMap"

    member this.TryNth(index: int) : option<'V> =
        try
            Some this.Values[index]
        with
        | :? ArgumentOutOfRangeException -> None

    member this.Exists(key: 'K) : bool = this.Mapping.ContainsKey key

    member this.DebugPrint() =
        for keyValuePair in this.Mapping do
            printfn "%A: %A" keyValuePair.Key this.Values[keyValuePair.Value]

    static member Empty = IndexedMap<'K, 'V>(Seq.empty, Seq.empty)

    interface Collections.IEnumerable with
        member this.GetEnumerator() =
            new IndexedMapEnumerator<'K, 'V>(this.Values.GetEnumerator(), this.Mapping.GetEnumerator())

    interface IEnumerable<'K * 'V> with
        member this.GetEnumerator() =
            new IndexedMapEnumerator<'K, 'V>(this.Values.GetEnumerator(), this.Mapping.GetEnumerator())

[<AutoOpen>]
module Waxt.TypeChecker.CheckAst

open System
open System.Collections.Generic
open Waxt.Type
open Waxt.TypedAst
open Waxt.UntypedAst

type IndexedMap<'K, 'V when 'K: comparison> private (values: seq<'V>, mapping: seq<'K * int>) =
    let keyIndexDict = new Dictionary<'K, int>()

    do mapping |> Seq.iter keyIndexDict.Add

    member val private Values = ResizeArray(values)

    member val private Mapping = keyIndexDict

    member this.Add(key: 'K, value: 'V) =
        this.Mapping.Add(key, this.Values.Count)
        this.Values.Add(value)

    member this.Item
        with get (key: 'K) = this.Values[this.Mapping[key]]

    member this.TryItem(key: 'K) : option<'V> =
        try
            Some this.Values[this.Mapping[key]]
        with
        | :? KeyNotFoundException -> None
        | :? ArgumentOutOfRangeException -> failwith "Fatal error: illegal state in IndexedMap"

    member this.ItemByIndex(index: int) = this.Values[index]

    member this.TryItemByIndex(index: int) : option<'V> =
        try
            Some this.Values[index]
        with
        | :? ArgumentOutOfRangeException -> None

    static member Empty = IndexedMap<'K, 'V>(Seq.empty, Seq.empty)

type FuncName = private FuncName of name: string

module FuncName =
    let make name = FuncName name

type FuncSig = private FuncSig of parameters: IndexedMap<string, Type> * result: Type

module FuncSig =
    let make parameters result = FuncSig(parameters, result)

let checkAst (stmts: list<Stmt>) : list<TypedExpr> = failwith "not implemented"

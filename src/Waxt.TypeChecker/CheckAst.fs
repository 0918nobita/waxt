[<AutoOpen>]
module Waxt.TypeChecker.CheckAst

open Waxt.Type
open Waxt.TypedAst
open Waxt.UntypedAst

type IndexedMap<'K, 'V when 'K: comparison> private (values: seq<'V>, mapping: seq<'K * int>) =
    let keyIndexDict = new System.Collections.Generic.Dictionary<'K, int>()

    do mapping |> Seq.iter keyIndexDict.Add

    member val private Values = ResizeArray(values)

    member val private Mapping = keyIndexDict

    member this.Add(key: 'K, value: 'V) =
        this.Mapping.Add(key, this.Values.Count)
        this.Values.Add(value)

    static member Empty = IndexedMap<'K, 'V>(Seq.empty, Seq.empty)

type FuncName = private FuncName of name: string

module FuncName =
    let make name = FuncName name

type FuncSig = private FuncSig of parameters: IndexedMap<string, Type> * result: Type

module FuncSig =
    let make parameters result = FuncSig(parameters, result)

let checkAst (stmts: list<Stmt>) : list<TypedExpr> = failwith "not implemented"

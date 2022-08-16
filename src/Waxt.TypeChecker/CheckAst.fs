[<AutoOpen>]
module Waxt.TypeChecker.CheckAst

open Waxt.Type
open Waxt.TypedAst
open Waxt.UntypedAst

type IndexedMap<'K, 'V when 'K: comparison> = private IndexedMapInner of values: list<'V> * mapping: Map<'K, int>

module IndexedMap =
    let empty<'K, 'V when 'K: comparison> : IndexedMap<'K, 'V> =
        IndexedMapInner([], Map.empty)

    let add<'K, 'V when 'K: comparison>
        (key: 'K)
        (value: 'V)
        ((IndexedMapInner (values, mapping)): IndexedMap<'K, 'V>)
        : IndexedMap<'K, 'V> =
        let index = List.length values
        let values = values @ [ value ]
        let mapping = Map.add key index mapping
        IndexedMapInner(values, mapping)

type FuncName = private FuncName of name: string

module FuncName =
    let make name = FuncName name

type FuncSig = private FuncSig of parameters: list<string * Type> * result: Type

let funcs =
    IndexedMap.empty<FuncName, FuncSig>
    |> IndexedMap.add (FuncName.make "foo") (FuncSig([], I32 None))

let checkAst (stmts: list<Stmt>) : list<TypedExpr> = failwith "not implemented"

[<AutoOpen>]
module Waxt.TypeChecker.CheckAst

open FsToolkit.ErrorHandling
open System
open System.Collections.Generic
open Waxt.Type
open Waxt.TypedAst
open Waxt.UntypedAst

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

type FuncName = private FuncName of name: string

module FuncName =
    let make name = FuncName name

type FuncSig = private FuncSig of parameters: IndexedMap<string, Type> * result: Type

module FuncSig =
    let make parameters result = FuncSig(parameters, result)

/// 各関数のシグネチャをチェック・取得する
let getFuncSigs (stmts: list<Stmt>) : Result<IndexedMap<FuncName, FuncSig * list<Expr>>, string> =
    let sigs = IndexedMap<FuncName, FuncSig * list<Expr>>.Empty

    stmts
    |> List.traverseResultM (fun (FuncDef (name, resultType, parameters, body)) ->
        let funcName = FuncName.make name

        if sigs.Exists funcName then
            Error "duplicate function name"
        else
            let funcParams = IndexedMap<string, Type>.Empty

            result {
                let! _ =
                    parameters
                    |> List.traverseResultM (fun (paramName, paramType) ->
                        if funcParams.Exists paramName then
                            Error "duplicate parameter"
                        else
                            funcParams.Add(paramName, paramType)
                            Ok())

                let funcSig = FuncSig(funcParams, resultType)
                sigs.Add(funcName, (funcSig, body))
                return ()
            })
    |> Result.map (fun _ -> sigs)

/// 各関数の本体を型付けする
let typeFuncBodies (untyped: IndexedMap<FuncName, FuncSig * Expr>) : IndexedMap<FuncName, FuncSig * list<TypedExpr>> =
    failwith "not implemented"

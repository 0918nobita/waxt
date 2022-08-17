[<AutoOpen>]
module Waxt.TypeChecker.CheckAst

open FsToolkit.ErrorHandling
open Waxt.Type
open Waxt.TypedAst
open Waxt.UntypedAst

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
            // TODO: もっと詳細なエラーメッセージを返す
            Error "duplicate function name"
        else
            let funcParams = IndexedMap<string, Type>.Empty

            result {
                let! _ =
                    parameters
                    |> List.traverseResultM (fun (paramName, paramType) ->
                        if funcParams.Exists paramName then
                            // TODO: もっと詳細なエラーメッセージを返す
                            Error "duplicate parameter"
                        else
                            funcParams.Add(paramName, paramType)
                            Ok())

                let funcSig = FuncSig(funcParams, resultType)
                sigs.Add(funcName, (funcSig, body))
                return ()
            })
    |> Result.map (fun _ -> sigs)

type UntypedFuncs = IndexedMap<FuncName, FuncSig * list<Expr>>
type TypedFuncs = IndexedMap<FuncName, FuncSig * list<TypedExpr>>

/// 各関数の本体を型付けする
let typeFuncBodies (untypedFuncs: UntypedFuncs) : TypedFuncs =
    let typedFuncs =
        IndexedMap<FuncName, FuncSig * list<TypedExpr>>
            .Empty

    for (funcName, (FuncSig (parameters, resultType), body)) in untypedFuncs do
        printfn "%A" funcName
        let parameters = Seq.toList parameters
        printfn "  parameters: %A" parameters
        printfn "  body: %A" body

        // TODO: 仮引数リストの内容を反映して型付けする
        body
        |> List.map (checkType TypeEnv.empty)
        |> printfn "  typedExprs: %A"

    typedFuncs

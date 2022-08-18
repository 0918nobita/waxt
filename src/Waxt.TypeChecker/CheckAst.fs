[<AutoOpen>]
module Waxt.TypeChecker.CheckAst

open FsToolkit.ErrorHandling
open Waxt.Location
open Waxt.Type
open Waxt.TypedAst
open Waxt.UntypedAst

type FuncSig = FuncSig of parameters: IndexedMap<string, Type> * result: Type * at: Range

type UntypedFuncs = IndexedMap<string, FuncSig * list<Expr>>

/// 各関数のシグネチャをチェック・取得する
let getFuncSigs (stmts: list<FuncDef>) : Result<UntypedFuncs, TypeError> =
    let sigs = IndexedMap<string, FuncSig * list<Expr>>.Empty

    stmts
    |> List.traverseResultM (fun (FuncDef (FuncName (name, at), resultType, parameters, body)) ->
        if sigs.Exists name then
            let (FuncSig (_, _, at), _) = sigs[name] |> Option.get
            Error(TypeError($"duplicate function name `%s{name}`", Some at))
        else
            let funcParams = IndexedMap<string, Type>.Empty

            result {
                let! _ =
                    parameters
                    |> List.traverseResultM (fun (paramName, paramType) ->
                        if funcParams.Exists paramName then
                            // TODO: もっと詳細なエラーメッセージを返す
                            Error(TypeError("duplicate parameter", None))
                        else
                            funcParams.Add(paramName, paramType)
                            Ok())

                let funcSig = FuncSig(funcParams, resultType, at)
                sigs.Add(name, (funcSig, body))
                return ()
            })
    |> Result.map (fun _ -> sigs)

type TypedFuncs = IndexedMap<string, FuncSig * list<TypedExpr>>

/// 各関数の本体を型付けする
let typeFuncBodies (untypedFuncs: UntypedFuncs) : TypedFuncs =
    let typedFuncs =
        IndexedMap<string, FuncSig * list<TypedExpr>>
            .Empty

    for (funcName, (FuncSig (parameters, resultType, at), body)) in untypedFuncs do
        printfn "%A" funcName
        let parameters = Seq.toList parameters
        printfn "  parameters: %A" parameters
        printfn "  body: %A" body

        // TODO: 仮引数リストの内容を反映して型付けする
        body
        |> List.map (checkType TypeEnv.empty)
        |> printfn "  typedExprs: %A"

    typedFuncs

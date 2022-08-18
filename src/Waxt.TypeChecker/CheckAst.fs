[<AutoOpen>]
module Waxt.TypeChecker.CheckAst

open FsToolkit.ErrorHandling
open Waxt.Location
open Waxt.Type
open Waxt.TypedAst
open Waxt.UntypedAst

type FuncSig = FuncSig of parameters: IndexedMap<string, Range * Type> * result: Type * at: Range

type UntypedFuncs = IndexedMap<string, FuncSig * list<Expr>>

/// 各関数のシグネチャをチェック・取得する
let getFuncSigs (stmts: list<FuncDef>) : Result<UntypedFuncs, TypeError> =
    let sigs = UntypedFuncs(100)

    let registerFuncSig (FuncDef (FuncName (name, at), resultType, parameters, body)) : Result<unit, TypeError> =
        if sigs.Exists name then
            let (FuncSig (_, _, at), _) = sigs[name] |> Option.get
            Error(TypeError($"Duplicate function name `%s{name}`", Some at))
        else
            let funcParams = IndexedMap<string, Range * Type>(10)

            let registerFuncParam (paramName, at, paramType) : Result<unit, TypeError> =
                if funcParams.Exists paramName then
                    Error(TypeError($"Duplicate parameter `%s{paramName}`", Some at))
                else
                    funcParams.Add(paramName, (at, paramType))
                    Ok()

            result {
                let! _ =
                    parameters
                    |> List.traverseResultM registerFuncParam

                let funcSig = FuncSig(funcParams, resultType, at)
                sigs.Add(name, (funcSig, body))
                return ()
            }

    stmts
    |> List.traverseResultM registerFuncSig
    |> Result.map (fun _ -> sigs)

type TypedFuncs = IndexedMap<string, FuncSig * list<TypedExpr>>

/// 各関数の本体を型付けする
let typeFuncBodies (untypedFuncs: UntypedFuncs) : TypedFuncs =
    let typedFuncs = TypedFuncs(100)

    for (funcName, (FuncSig (parameters, resultType, at), body)) in untypedFuncs do
        printfn "%s" funcName
        let parameters = Seq.toList parameters
        printfn "  parameters: %A" parameters
        printfn "  body: %A" body

        // TODO: 仮引数リストの内容を反映して型付けする
        body
        |> List.map (checkType TypeEnv.empty)
        |> printfn "  typedExprs: %A"

    typedFuncs

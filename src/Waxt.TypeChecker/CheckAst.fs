[<AutoOpen>]
module Waxt.TypeChecker.CheckAst

open FsToolkit.ErrorHandling
open Waxt.Location
open Waxt.Type
open Waxt.TypedAst
open Waxt.UntypedAst

module SeqExt =
    let iterWhileOk (f: 't -> Result<unit, 'e>) (sequence: seq<'t>) : Result<unit, 'e> =
        let folder (state: Result<unit, 'e>) (item: 't) = state |> Result.bind (fun () -> f item)

        sequence |> Seq.fold folder (Ok())

type FuncSig = FuncSig of parameters: IndexedMap<string, Range * Type> * result: Type * at: Range

type UntypedFuncs = IndexedMap<string, FuncSig * list<Expr>>

/// 関数の仮引数リストをチェックし IndexedMap として取得する
let checkFuncParams parameters =
    let funcParams = IndexedMap<string, Range * Type>(10)

    let registerFuncParam (paramName, at, paramType) : Result<unit, TypeError> =
        result {
            do!
                funcParams[paramName]
                |> Result.requireNone (TypeError($"Duplicate parameter `%s{paramName}`", at))

            funcParams.Add(paramName, (at, paramType))
        }

    result {
        do! parameters |> SeqExt.iterWhileOk registerFuncParam
        return funcParams
    }

/// 各関数のシグネチャをチェックし IndexedMap として取得する
let checkFuncSigs funcDefs =
    let sigs = UntypedFuncs(100)

    let registerFuncSig (FuncDef (FuncName (name, at), resultType, parameters, body)) : Result<unit, TypeError> =
        result {
            do!
                sigs[name]
                |> Result.requireNone (TypeError($"Duplicate function name `%s{name}`", at))

            let! funcParams = checkFuncParams parameters

            let funcSig = FuncSig(funcParams, resultType, at)
            sigs.Add(name, (funcSig, body))
        }

    result {
        do! funcDefs |> SeqExt.iterWhileOk registerFuncSig
        return sigs
    }

type TypedFuncs = IndexedMap<string, FuncSig * list<TypedExpr>>

let rec private checkProgn
    (funcNameRange: Range)
    (expectedResultTy: Type)
    (exprTypes: list<Range * Type>)
    : Result<unit, TypeError> =
    match exprTypes with
    | [] -> Error(TypeError("The result type is not unit, but no expression is found", funcNameRange))
    | [ range, ty ] -> expectType expectedResultTy (ty, range)
    | (range, ty) :: rest ->
        result {
            do! expectType Unit (ty, range)
            do! checkProgn funcNameRange expectedResultTy rest
        }

/// 各関数の本体を型付けする
let typeFuncBodies (untypedFuncs: UntypedFuncs) : Result<TypedFuncs, TypeError> =
    let typedFuncs = TypedFuncs(untypedFuncs.Count)

    result {
        do!
            untypedFuncs
            |> SeqExt.iterWhileOk (fun (funcName, (funcSig, body)) ->
                let (FuncSig (parameters, resultTy, funcNameRange)) = funcSig

                let typeEnv =
                    parameters
                    |> Seq.map (fun (name, (at, ty)) -> (name, (ty, at)))
                    |> TypeEnv.ofSeq

                result {
                    let! typedBody =
                        body
                        |> List.map (checkType typeEnv)
                        |> List.sequenceResultM

                    do!
                        checkProgn
                            funcNameRange
                            resultTy
                            (typedBody
                             |> List.map (fun typedExpr ->
                                 ((typedExpr :> ILocatable).Locate(), TypedExpr.getType typedExpr)))

                    typedFuncs.Add(funcName, (funcSig, typedBody))
                })

        return typedFuncs
    }

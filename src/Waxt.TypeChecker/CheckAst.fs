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

/// 関数の仮引数リストをチェックし IndexedMap として取得する
let checkFuncParams parameters : Result<FuncParams, TypeError> =
    let funcParams = FuncParams(10)

    let registerFuncParam ((paramName, paramNameRange), (paramType, paramTypeRange)) : Result<unit, TypeError> =
        result {
            do!
                funcParams[paramName]
                |> Result.requireNone (TypeError($"Duplicate parameter `%s{paramName}`", paramNameRange))

            funcParams.Add(paramName, (paramNameRange, (paramType, paramTypeRange)))
        }

    result {
        do! parameters |> SeqExt.iterWhileOk registerFuncParam
        return funcParams
    }

type UntypedFuncs = IndexedMap<string, FuncSig * list<Expr>>

/// 各関数のシグネチャをチェックし IndexedMap として取得する
let checkFuncSigs funcDefs : Result<UntypedFuncs, TypeError> =
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

let rec private checkProgn
    (funcNameRange: Range)
    (expectedResultTy: Type)
    (exprTypes: list<Type * Range>)
    : Result<unit, TypeError> =
    match exprTypes with
    | [] -> Error(TypeError("The result type is not unit, but no expression is found", funcNameRange))
    | [ ty, range ] -> expectType expectedResultTy (ty, range)
    | (ty, range) :: rest ->
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
                    |> Seq.map (fun (name, (nameRange, (ty, tyRange))) -> ((name, nameRange), (ty, tyRange)))
                    |> Seq.fold (fun state (name, ty) -> TypeEnv.add name ty state) TypeEnv.empty

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
                                 TypedExpr.getType typedExpr, (typedExpr :> ILocatable).Locate()))

                    typedFuncs.Add(funcName, (funcSig, typedBody))
                })

        return typedFuncs
    }

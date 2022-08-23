[<AutoOpen>]
module Waxt.TypeChecker.TypeFuncSigs

open FsToolkit.ErrorHandling
open Waxt.TypedAst
open Waxt.UntypedAst

/// 関数の仮引数リストをチェックし IndexedMap として取得する
let typeFuncParams parameters : Result<FuncParams, TypeError> =
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
let typeFuncSigs funcDefs : Result<UntypedFuncs, TypeError> =
    let sigs = UntypedFuncs(100)

    let registerFuncSig (FuncDef (FuncName (name, at), resultType, parameters, body)) : Result<unit, TypeError> =
        result {
            do!
                sigs[name]
                |> Result.requireNone (TypeError($"Duplicate function name `%s{name}`", at))

            let! funcParams = typeFuncParams parameters

            let funcSig = FuncSig(funcParams, resultType, at)
            sigs.Add(name, (funcSig, body))
        }

    result {
        do! funcDefs |> SeqExt.iterWhileOk registerFuncSig
        return sigs
    }

[<AutoOpen>]
module Waxt.TypeChecker.TypeFuncBodies

open FsToolkit.ErrorHandling
open Waxt.Location
open Waxt.Type
open Waxt.TypedAst

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
                        |> List.map (typeExpr typeEnv)
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

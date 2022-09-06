module Waxt.TypeInferrer.Extract

open FsToolkit.ErrorHandling
open TypeEquation
open Waxt.Ast
open Waxt.Location
open Waxt.Token
open Waxt.Type

let private typeLiteralToType (typeLiteral: TypeLiteral) =
    match typeLiteral with
    | TypeLiteral.I32 -> NumType NumType.I32
    | TypeLiteral.I64 -> NumType NumType.I64
    | TypeLiteral.F32 -> NumType NumType.F32
    | TypeLiteral.F64 -> NumType NumType.F64

let rec extract
    (funcContext: FuncContext)
    (varContext: VarContext)
    (expr: MutableExpr)
    : Result<TypeSimulEquation * Type, list<string>> =
    match expr with
    | I32Const _ -> Ok(TypeSimulEquation.empty, NumType NumType.I32)

    | I32Eqz (t, at) ->
        result {
            let! (equation, ty) = extract funcContext varContext t

            let equation =
                equation
                |> TypeSimulEquation.add ty (NumType NumType.I32) at

            return (equation, NumType NumType.I32)
        }

    | I32Add (lhs, op, rhs) -> extractFromBinExpr funcContext varContext lhs (I32AddOp.locate op) rhs
    | I32Sub (lhs, op, rhs) -> extractFromBinExpr funcContext varContext lhs (I32SubOp.locate op) rhs
    | I32Mul (lhs, op, rhs) -> extractFromBinExpr funcContext varContext lhs (I32MulOp.locate op) rhs

    | If ifExpr ->
        result {
            let! (e1, _) = extract funcContext varContext (IfExpr.cond ifExpr)
            let! (e2, ty2) = extractFromBlock funcContext varContext (IfExpr.thenClause ifExpr)
            let! (e3, ty3) = extractFromBlock funcContext varContext (IfExpr.elseClause ifExpr)

            let e =
                TypeSimulEquation.combine e1 e2
                |> TypeSimulEquation.combine e3
                |> TypeSimulEquation.add ty2 ty3 (IfKeyword.locate (IfExpr.ifKeyword ifExpr))

            return (e, ty2)
        }

    | Let (name, value, body, _) ->
        result {
            let! (e1, ty1) = extract funcContext varContext value
            let! (e2, ty2) = extract funcContext (VarContext.add name ty1 varContext) body
            let e = TypeSimulEquation.combine e1 e2
            return (e, ty2)
        }

    | LetWithType (name, tyLit, value, body, at) ->
        result {
            let! (e1, ty1) = extract funcContext varContext value
            let! (e2, ty2) = extract funcContext (VarContext.add name ty1 varContext) body

            let e =
                TypeSimulEquation.combine e1 e2
                |> TypeSimulEquation.add ty1 (typeLiteralToType tyLit) at

            return (e, ty2)
        }

    | Application (funcName, args, at) ->
        result {
            let! (FuncType (argTypes, retType)) =
                funcContext
                |> FuncContext.tryFind funcName
                |> Result.requireSome [ $"%O{funcName} is not defined" ]

            let! args =
                args
                |> List.map (extract funcContext varContext)
                |> List.sequenceResultM

            if List.length args <> List.length argTypes then
                return! Error [ "Arity mismatch" ]
            else
                let equation =
                    (args
                     |> List.mapi (fun i (e, ty) -> TypeSimulEquation.add ty argTypes.[i] at e))
                    |> TypeSimulEquation.combineMany

                return (equation, retType)
        }

    | Var (name, tyRef, _) ->
        result {
            let! ty =
                varContext
                |> VarContext.tryFind name
                |> Result.requireSome [ $"%O{name} is not defined" ]

            tyRef.Value <- Some ty

            return (TypeSimulEquation.empty, ty)
        }

and private extractFromBinExpr
    (funcContext: FuncContext)
    (varContext: VarContext)
    (lhs: MutableExpr)
    (opLoc: Range)
    (rhs: MutableExpr)
    =
    result {
        let! (e1, ty1) = extract funcContext varContext lhs
        let! (e2, ty2) = extract funcContext varContext rhs

        let e3 =
            TypeSimulEquation.combine e1 e2
            |> TypeSimulEquation.add ty1 (NumType NumType.I32) opLoc
            |> TypeSimulEquation.add ty2 (NumType NumType.I32) opLoc

        return (e3, NumType NumType.I32)
    }

and private extractFromBlock (funcContext: FuncContext) (varContext: VarContext) (block: MutableBlock) =
    result {
        let! e =
            block
            |> Block.precedingBody
            |> List.map (fun expr ->
                result {
                    let! (e, ty) = extract funcContext varContext expr

                    return
                        e
                        |> TypeSimulEquation.add ty Void ((expr :> IExpr).locate ())
                })
            |> List.sequenceResultA
            |> Result.map TypeSimulEquation.combineMany
            |> Result.mapError List.concat

        let! (e', ty) = extract funcContext varContext (Block.lastBody block)
        return (TypeSimulEquation.combine e e', ty)
    }

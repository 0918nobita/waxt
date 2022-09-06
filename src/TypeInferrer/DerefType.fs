module Waxt.TypeInferrer.DerefType

open FsToolkit.ErrorHandling
open Waxt.Ast
open Waxt.Type

let rec derefType (assigns: list<Assign>) (expr: MutableExpr) : Result<FixedExpr, list<TypeInferenceError>> =
    match expr with
    | I32Const (n, at) -> Ok(I32Const(n, at))

    | I32Eqz (t, at) ->
        result {
            let! t = derefType assigns t
            return I32Eqz(t, at)
        }

    | I32Add (lhs, opLoc, rhs) ->
        result {
            let! lhs = derefType assigns lhs
            let! rhs = derefType assigns rhs
            return I32Add(lhs, opLoc, rhs)
        }

    | I32Sub (lhs, opLoc, rhs) ->
        result {
            let! lhs = derefType assigns lhs
            let! rhs = derefType assigns rhs
            return I32Sub(lhs, opLoc, rhs)
        }

    | I32Mul (lhs, opLoc, rhs) ->
        result {
            let! lhs = derefType assigns lhs
            let! rhs = derefType assigns rhs
            return I32Mul(lhs, opLoc, rhs)
        }

    | If ifExpr ->
        result {
            let! cond = derefType assigns (IfExpr.cond ifExpr)
            let! thenClause = derefTypeInBlock assigns (IfExpr.thenClause ifExpr)
            let! elseClause = derefTypeInBlock assigns (IfExpr.elseClause ifExpr)

            return If(IfExpr.make (IfExpr.ifKeyword ifExpr) cond thenClause elseClause)
        }

    | Let (varName, boundValue, body, at) ->
        result {
            let! boundValue = derefType assigns boundValue
            let! body = derefType assigns body
            return Let(varName, boundValue, body, at)
        }

    | LetWithType (varName, ty, boundValue, body, at) ->
        result {
            let! boundValue = derefType assigns boundValue
            let! body = derefType assigns body
            return LetWithType(varName, ty, boundValue, body, at)
        }

    | Application (funcName, args, at) ->
        result {
            let! args =
                args
                |> List.map (derefType assigns)
                |> List.sequenceResultA
                |> Result.mapError (fun err -> List.concat err)

            return Application(funcName, args, at)
        }

    | Var (varName, tyRef, at) ->
        result {
            let! ty =
                tyRef.Value
                |> Result.requireSome [ TypeInferenceError("(Fatal) Failed to get type information", at) ]

            let! ty =
                match ty with
                | TyVar tyVarName ->
                    assigns
                    |> List.tryFind (fun (Assign (tyVarName', _)) -> tyVarName = tyVarName')
                    |> Option.map (fun (Assign (_, ty')) -> ty')
                    |> Result.requireSome [ TypeInferenceError($"%O{tyVarName} was not resolved", at) ]
                | _ -> Ok ty

            return Var(varName, ty, at)
        }

and private derefTypeInBlock (assigns: list<Assign>) (block: MutableBlock) =
    result {
        let! preceding =
            block
            |> Block.precedingBody
            |> List.map (derefType assigns)
            |> List.sequenceResultA
            |> Result.mapError List.concat

        let! last = derefType assigns (Block.lastBody block)
        return Block.make (Block.openBrace block) preceding last (Block.closeBrace block)
    }

[<AutoOpen>]
module Waxt.TypeChecker.CheckExpr

open FsToolkit.ErrorHandling
open Waxt.Location
open Waxt.Type
open Waxt.TypedAst
open Waxt.UntypedAst

let rec checkType (typeEnv: TypeEnv) : Expr -> Result<TypedExpr, TypeError> =
    function
    | I32Add (lhs, rhs, at) ->
        result {
            let! lhs = checkType typeEnv lhs
            do! expectType I32 (TypedExpr.getType lhs, (lhs :> ILocatable).Locate())
            let! rhs = checkType typeEnv rhs
            do! expectType I32 (TypedExpr.getType rhs, (rhs :> ILocatable).Locate())
            return TypedExpr.I32Add(lhs, rhs, at)
        }

    | I32Const (value, at) -> Ok(TypedExpr.I32Const(value, at))

    | I32Mul (lhs, rhs, at) ->
        result {
            let! lhs = checkType typeEnv lhs
            do! expectType I32 (TypedExpr.getType lhs, (lhs :> ILocatable).Locate())
            let! rhs = checkType typeEnv rhs
            do! expectType I32 (TypedExpr.getType rhs, (rhs :> ILocatable).Locate())
            return TypedExpr.I32Mul(lhs, rhs, at)
        }

    | I32Store (addr, content, at) ->
        result {
            let! addr = checkType typeEnv addr
            do! expectType I32 (TypedExpr.getType addr, (addr :> ILocatable).Locate())
            let! content = checkType typeEnv content
            do! expectType I32 (TypedExpr.getType content, (content :> ILocatable).Locate())
            return TypedExpr.I32Store(addr, content, at)
        }

    | Var (name, at) ->
        result {
            let! (index, (ty, _)) =
                TypeEnv.find name typeEnv
                |> Result.requireSome (TypeError($"{name} is not defined", at))

            return TypedExpr.Var(index, ty, at)
        }

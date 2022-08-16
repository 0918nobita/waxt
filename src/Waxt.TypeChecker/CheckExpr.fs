[<AutoOpen>]
module Waxt.TypeChecker.CheckExpr

open FsToolkit.ErrorHandling
open Waxt.Type
open Waxt.TypedAst
open Waxt.UntypedAst

let rec checkType (typeEnv: TypeEnv) : Expr -> Result<TypedExpr, TypeError> =
    function
    | I32Add (lhs, rhs, at) ->
        result {
            let! lhs = checkType typeEnv lhs
            do! expectType (I32 None) (TypedExpr.getType lhs)
            let! rhs = checkType typeEnv rhs
            do! expectType (I32 None) (TypedExpr.getType rhs)
            return TypedExpr.I32Add(lhs, rhs, at)
        }

    | I32Const (value, at) -> Ok(TypedExpr.I32Const(value, at))

    | I32Mul (lhs, rhs, at) ->
        result {
            let! lhs = checkType typeEnv lhs
            do! expectType (I32 None) (TypedExpr.getType lhs)
            let! rhs = checkType typeEnv rhs
            do! expectType (I32 None) (TypedExpr.getType rhs)
            return TypedExpr.I32Mul(lhs, rhs, at)
        }

    | I32Store (addr, content, at) ->
        result {
            let! addr = checkType typeEnv addr
            do! expectType (I32 None) (TypedExpr.getType addr)
            let! content = checkType typeEnv content
            do! expectType (I32 None) (TypedExpr.getType content)
            return TypedExpr.I32Store(addr, content, at)
        }

    | Var (name, at) ->
        result {
            let! ty =
                TypeEnv.find name typeEnv
                |> Result.requireSome (TypeError($"{name} is not defined", Some at))

            return TypedExpr.Var(name, ty, at)
        }

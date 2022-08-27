module Waxt.TypeInferrer.Unify

open FsToolkit.ErrorHandling
open Type
open TypeEquation

type Assign = Assign of varName: string * ty: Type

let rec private unify' (equations: list<Type * Type>) : Result<list<Assign>, string> =
    match equations with
    | [] -> Ok []

    | (ty1, ty2) :: rest when ty1 = ty2 -> unify' rest

    | ((TyVar name, ty)

    | (ty, TyVar name)) :: rest ->
        let frv = Type.freeTypeVars ty

        if List.contains name frv then
            Error $"Failed to unify type variable %s{name}"
        else
            result {
                let! assigns = unify' (assign name ty rest)
                return Assign(name, ty) :: assigns
            }

    | (Func (FuncType (args, ret)), Func (FuncType (args', ret'))) :: rest ->
        if List.length args <> List.length args' then
            Error "Arity mismatch"
        else
            args
            |> List.mapi (fun i ty -> (ty, args'.[i]))
            |> unify'

    | equation -> Error $"Cannot solve %O{equation}"

let unify (equations: seq<Type * Type>) =
    equations
    |> List.ofSeq
    |> unify'
    |> Result.map List.distinct

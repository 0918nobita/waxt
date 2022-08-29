module WAXT.TypeInferrer.Unify

open FsToolkit.ErrorHandling
open WAXT.Type
open TypeEquation

type Assign = Assign of TyVarName * Type

let rec private unify' (equations: list<TypeEquation>) : Result<list<Assign>, string> =
    match equations with
    | [] -> Ok []

    | TypeEquation (ty1, ty2, _) :: rest when ty1 = ty2 -> unify' rest

    | (TypeEquation (TyVar name, ty, _)

    | TypeEquation (ty, TyVar name, _)) :: rest ->
        let frv = Type.freeTypeVars ty

        if List.contains name frv then
            Error $"Failed to unify type variable %O{name}"
        else
            result {
                let! assigns = unify' (assign name ty rest)
                return Assign(name, ty) :: assigns
            }

    | TypeEquation (Func (FuncType (args, ret) as func1), Func (FuncType (args', ret') as func2), at) :: rest ->
        if List.length args <> List.length args' then
            Error $"Arity mismatch: %O{func1} and %O{func2}"
        else
            TypeEquation(ret, ret', at) :: rest
            @ (args
               |> List.mapi (fun i ty -> TypeEquation(ty, args'.[i], at)))
            |> unify'

    | equation :: _ -> Error $"Cannot solve %O{equation}"

let unify (equations: TypeSimulEquation) =
    equations
    |> List.ofSeq
    |> unify'
    |> Result.map List.distinct

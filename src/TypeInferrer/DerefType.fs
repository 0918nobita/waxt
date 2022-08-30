module WAXT.TypeInferrer.DerefType

open FsToolkit.ErrorHandling
open WAXT.AST
open WAXT.Type

let rec derefType (assigns: list<Assign>) (term: MutableTerm) : Result<DereferencedTerm, list<TypeInferenceError>> =
    match term with
    | I32Const (n, at) -> Ok(I32Const(n, at))

    | I32Eqz (t, at) ->
        result {
            let! t = derefType assigns t
            return I32Eqz(t, at)
        }

    | I32Add (lhs, rhs, at) ->
        result {
            let! lhs = derefType assigns lhs
            let! rhs = derefType assigns rhs
            return I32Add(lhs, rhs, at)
        }

    | I32Sub (lhs, rhs, at) ->
        result {
            let! lhs = derefType assigns lhs
            let! rhs = derefType assigns rhs
            return I32Sub(lhs, rhs, at)
        }

    | I32Mul (lhs, rhs, at) ->
        result {
            let! lhs = derefType assigns lhs
            let! rhs = derefType assigns rhs
            return I32Mul(lhs, rhs, at)
        }

    | If (cond, thenClause, elseClause, at) ->
        result {
            let! cond = derefType assigns cond
            let! thenClause = derefType assigns thenClause
            let! elseClause = derefType assigns elseClause
            return If(cond, thenClause, elseClause, at)
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

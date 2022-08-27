[<AutoOpen>]
module Waxt.TypeInferrer.Extract

open FsToolkit.ErrorHandling

let rec extract (context: Context) (term: Term) : Result<TypeSimulEquation * Type, string> =
    match term with
    | I32Const _ -> Ok(TypeSimulEquation.empty, I32)

    | I32Eqz t ->
        result {
            let! (equation, ty) = extract context t
            let equation = equation |> TypeSimulEquation.addEquation ty I32
            return (equation, I32)
        }

    | I32Add (lhs, rhs)
    | I32Sub (lhs, rhs)
    | I32Mul (lhs, rhs) ->
        result {
            let! (e1, ty1) = extract context lhs
            let! (e2, ty2) = extract context rhs

            let e3 =
                TypeSimulEquation.combine e1 e2
                |> TypeSimulEquation.addEquation ty1 I32
                |> TypeSimulEquation.addEquation ty2 I32

            return (e3, I32)
        }

    | If (cond, thenClause, elseClause) ->
        result {
            let! (e1, _) = extract context cond
            let! (e2, ty2) = extract context thenClause
            let! (e3, ty3) = extract context elseClause

            let e =
                TypeSimulEquation.combine e1 e2
                |> TypeSimulEquation.combine e3
                |> TypeSimulEquation.addEquation ty2 ty3

            return (e, ty2)
        }

    | Let (name, value, body) ->
        result {
            let! (e1, ty1) = extract context value
            let! (e2, ty2) = extract (Context.add name ty1 context) body
            let e = TypeSimulEquation.combine e1 e2
            return (e, ty2)
        }

    | LetWithType (name, tyLit, value, body) ->
        result {
            let! (e1, ty1) = extract context value
            let! (e2, ty2) = extract (Context.add name ty1 context) body

            let e =
                TypeSimulEquation.combine e1 e2
                |> TypeSimulEquation.addEquation ty1 (Type.fromLiteral tyLit)

            return (e, ty2)
        }

    | Application (funcName, args) ->
        result {
            let! funcType =
                context
                |> Context.tryFind funcName
                |> Result.requireSome $"%s{funcName} is not defined"

            match funcType with
            | Func (argTypes, retType) ->
                let! args =
                    args
                    |> List.map (extract context)
                    |> List.sequenceResultM

                if List.length args <> List.length argTypes then
                    return! Error "Arity mismatch"
                else
                    let equation =
                        (args
                         |> List.mapi (fun i (e, ty) -> TypeSimulEquation.addEquation ty argTypes.[i] e))
                        |> TypeSimulEquation.combineMany

                    return (equation, retType)
            | _ -> return! Error "This is not a function"
        }

    | Var name ->
        result {
            let! ty =
                context
                |> Context.tryFind name
                |> Result.requireSome $"%s{name} is not defined"

            return (TypeSimulEquation.empty, ty)
        }

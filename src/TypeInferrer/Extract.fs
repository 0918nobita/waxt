module Waxt.TypeInferrer.Extract

open FsToolkit.ErrorHandling
open TypeEquation

let rec extract
    (funcContext: FuncContext)
    (varContext: VarContext)
    (term: Term)
    : Result<TypeSimulEquation * Type, string> =
    match term with
    | I32Const _ -> Ok(TypeSimulEquation.empty, I32)

    | I32Eqz t ->
        result {
            let! (equation, ty) = extract funcContext varContext t
            let equation = equation |> TypeSimulEquation.addEquation ty I32
            return (equation, I32)
        }

    | I32Add (lhs, rhs)
    | I32Sub (lhs, rhs)
    | I32Mul (lhs, rhs) ->
        result {
            let! (e1, ty1) = extract funcContext varContext lhs
            let! (e2, ty2) = extract funcContext varContext rhs

            let e3 =
                TypeSimulEquation.combine e1 e2
                |> TypeSimulEquation.addEquation ty1 I32
                |> TypeSimulEquation.addEquation ty2 I32

            return (e3, I32)
        }

    | If (cond, thenClause, elseClause) ->
        result {
            let! (e1, _) = extract funcContext varContext cond
            let! (e2, ty2) = extract funcContext varContext thenClause
            let! (e3, ty3) = extract funcContext varContext elseClause

            let e =
                TypeSimulEquation.combine e1 e2
                |> TypeSimulEquation.combine e3
                |> TypeSimulEquation.addEquation ty2 ty3

            return (e, ty2)
        }

    | Let (name, value, body) ->
        result {
            let! (e1, ty1) = extract funcContext varContext value
            let! (e2, ty2) = extract funcContext (VarContext.add name ty1 varContext) body
            let e = TypeSimulEquation.combine e1 e2
            return (e, ty2)
        }

    | LetWithType (name, tyLit, value, body) ->
        result {
            let! (e1, ty1) = extract funcContext varContext value
            let! (e2, ty2) = extract funcContext (VarContext.add name ty1 varContext) body

            let e =
                TypeSimulEquation.combine e1 e2
                |> TypeSimulEquation.addEquation ty1 (Type.fromLiteral tyLit)

            return (e, ty2)
        }

    | Application (funcName, args) ->
        result {
            let! (FuncType (argTypes, retType)) =
                funcContext
                |> FuncContext.tryFind funcName
                |> Result.requireSome $"%O{funcName} is not defined"

            let! args =
                args
                |> List.map (extract funcContext varContext)
                |> List.sequenceResultM

            if List.length args <> List.length argTypes then
                return! Error "Arity mismatch"
            else
                let equation =
                    (args
                     |> List.mapi (fun i (e, ty) -> TypeSimulEquation.addEquation ty argTypes.[i] e))
                    |> TypeSimulEquation.combineMany

                return (equation, retType)
        }

    | Var name ->
        result {
            let! ty =
                varContext
                |> VarContext.tryFind name
                |> Result.requireSome $"%O{name} is not defined"

            return (TypeSimulEquation.empty, ty)
        }

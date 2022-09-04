module WAXT.TypeInferrer.Extract

open FsToolkit.ErrorHandling
open TypeEquation
open WAXT.AST
open WAXT.Location
open WAXT.Type

let private typeLiteralToType (typeLiteral: TypeLiteral) =
    match typeLiteral with
    | TypeLiteral.I32 -> NumType NumType.I32
    | TypeLiteral.I64 -> NumType NumType.I64
    | TypeLiteral.F32 -> NumType NumType.F32
    | TypeLiteral.F64 -> NumType NumType.F64

let rec extract
    (funcContext: FuncContext)
    (varContext: VarContext)
    (term: MutableTerm)
    : Result<TypeSimulEquation * Type, string> =
    match term with
    | I32Const _ -> Ok(TypeSimulEquation.empty, NumType NumType.I32)

    | I32Eqz (t, at) ->
        result {
            let! (equation, ty) = extract funcContext varContext t

            let equation =
                equation
                |> TypeSimulEquation.addEquation ty (NumType NumType.I32) at

            return (equation, NumType NumType.I32)
        }

    | I32Add (lhs, op, rhs) -> extractFromBinExpr funcContext varContext lhs ((op :> ILocatable).Locate()) rhs
    | I32Sub (lhs, op, rhs) -> extractFromBinExpr funcContext varContext lhs ((op :> ILocatable).Locate()) rhs
    | I32Mul (lhs, op, rhs) -> extractFromBinExpr funcContext varContext lhs ((op :> ILocatable).Locate()) rhs

    | If (cond, thenClause, elseClause, at) ->
        result {
            let! (e1, _) = extract funcContext varContext cond
            let! (e2, ty2) = extract funcContext varContext thenClause
            let! (e3, ty3) = extract funcContext varContext elseClause

            let e =
                TypeSimulEquation.combine e1 e2
                |> TypeSimulEquation.combine e3
                |> TypeSimulEquation.addEquation ty2 ty3 at

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
                |> TypeSimulEquation.addEquation ty1 (typeLiteralToType tyLit) at

            return (e, ty2)
        }

    | Application (funcName, args, at) ->
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
                     |> List.mapi (fun i (e, ty) -> TypeSimulEquation.addEquation ty argTypes.[i] at e))
                    |> TypeSimulEquation.combineMany

                return (equation, retType)
        }

    | Var (name, tyRef, _) ->
        result {
            let! ty =
                varContext
                |> VarContext.tryFind name
                |> Result.requireSome $"%O{name} is not defined"

            tyRef.Value <- Some ty

            return (TypeSimulEquation.empty, ty)
        }

and private extractFromBinExpr
    (funcContext: FuncContext)
    (varContext: VarContext)
    (lhs: MutableTerm)
    (opLoc: Range)
    (rhs: MutableTerm)
    =
    result {
        let! (e1, ty1) = extract funcContext varContext lhs
        let! (e2, ty2) = extract funcContext varContext rhs

        let e3 =
            TypeSimulEquation.combine e1 e2
            |> TypeSimulEquation.addEquation ty1 (NumType NumType.I32) opLoc
            |> TypeSimulEquation.addEquation ty2 (NumType NumType.I32) opLoc

        return (e3, NumType NumType.I32)
    }

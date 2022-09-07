module Waxt.TypeInferrer.Extract

open FsToolkit.ErrorHandling
open TypeEquation
open Waxt.Ast
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

    | I32Eqz (Parenthesized inner as parenthesized) ->
        result {
            let! (equation, ty) = extract funcContext varContext inner.Arg

            let equation =
                equation
                |> TypeSimulEquation.add ty (NumType NumType.I32) (Parenthesized.locate parenthesized)

            return (equation, NumType NumType.I32)
        }

    | I32Add (Parenthesized inner)
    | I32Sub (Parenthesized inner)
    | I32Mul (Parenthesized inner) ->
        result {
            let! (e1, ty1) = extract funcContext varContext inner.Lhs
            let! (e2, ty2) = extract funcContext varContext inner.Rhs

            let e =
                TypeSimulEquation.combine e1 e2
                |> TypeSimulEquation.add ty1 (NumType NumType.I32) ((inner.Lhs :> IExpr).locate ())
                |> TypeSimulEquation.add ty2 (NumType NumType.I32) ((inner.Rhs :> IExpr).locate ())

            return (e, NumType NumType.I32)
        }

    | If ifExpr ->
        result {
            let! (e1, _) = extract funcContext varContext (IfExpr.cond ifExpr)
            let! (e2, ty2) = extractFromBlock funcContext varContext (IfExpr.thenClause ifExpr)
            let! (e3, ty3) = extractFromBlock funcContext varContext (IfExpr.elseClause ifExpr)

            let e =
                TypeSimulEquation.combine e1 e2
                |> TypeSimulEquation.combine e3
                |> TypeSimulEquation.add ty2 ty3 (Ident.locate (IfExpr.ifIdent ifExpr))

            return (e, ty2)
        }

    | Let (Parenthesized inner) ->
        result {
            let! (e1, ty1) = extract funcContext varContext inner.BoundValue
            let! (e2, ty2) = extract funcContext (VarContext.add inner.VarName ty1 varContext) inner.Body
            let e = TypeSimulEquation.combine e1 e2
            return (e, ty2)
        }

    | LetWithType (Parenthesized inner) ->
        result {
            let! (e1, ty1) = extract funcContext varContext inner.BoundValue
            let! (e2, ty2) = extract funcContext (VarContext.add inner.VarName ty1 varContext) inner.Body

            let e =
                TypeSimulEquation.combine e1 e2
                |> TypeSimulEquation.add ty1 (typeLiteralToType inner.TypeAnnotation) ((inner.Body :> IExpr).locate ())

            return (e, ty2)
        }

    | Application (Parenthesized inner) ->
        result {
            let! (FuncType (argTypes, retType)) =
                funcContext
                |> FuncContext.tryFind inner.FuncName
                |> Result.requireSome [ $"%O{inner.FuncName} is not defined" ]

            let! extractResults =
                inner.Args
                |> List.map (extract funcContext varContext)
                |> List.sequenceResultM

            if List.length extractResults <> List.length argTypes then
                return! Error [ "Arity mismatch" ]
            else
                let equation =
                    (extractResults
                     |> List.mapi (fun i (e, ty) ->
                         TypeSimulEquation.add ty argTypes.[i] ((inner.Args.[i] :> IExpr).locate ()) e))
                    |> TypeSimulEquation.combineMany

                return (equation, retType)
        }

    | Var (name, tyRef) ->
        result {
            let! ty =
                varContext
                |> VarContext.tryFind name
                |> Result.requireSome [ $"%O{name} is not defined" ]

            tyRef.Value <- Some ty

            return (TypeSimulEquation.empty, ty)
        }

and private extractFromBlock (funcContext: FuncContext) (varContext: VarContext) (block: MutableBlock) =
    result {
        let body = Block.body block

        let (precedingBody, lastBody) =
            match List.tryLast body with
            | Some last -> (List.take (List.length body - 1) body, Some last)
            | None -> ([], None)

        let! equations =
            precedingBody
            |> List.map (fun expr ->
                result {
                    let! (e, ty) = extract funcContext varContext expr

                    return
                        e
                        |> TypeSimulEquation.add ty Void ((expr :> IExpr).locate ())
                })
            |> List.sequenceResultA
            |> Result.mapError List.concat

        let equation = TypeSimulEquation.combineMany equations

        return!
            lastBody
            |> Option.map (fun lastBody ->
                result {
                    let! (e, ty) = extract funcContext varContext lastBody
                    return (equation |> TypeSimulEquation.combine e, ty)
                })
            |> Option.defaultValue (Ok(equation, Void))
    }

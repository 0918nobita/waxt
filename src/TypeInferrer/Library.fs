module Waxt.TypeInferrer

open FsToolkit.ErrorHandling

type TypeLiteral =
    | I32TyLit
    | I64TyLit
    | F32TyLit
    | F64TyLit

type Type =
    | I32
    | I64
    | F32
    | F64
    | TyVar of name: string
    | Func of args: list<Type> * ret: Type

module Type =
    let fromLiteral (lit: TypeLiteral) =
        match lit with
        | I32TyLit -> I32
        | I64TyLit -> I64
        | F32TyLit -> F32
        | F64TyLit -> F64

type Term =
    | I32Const of n: int
    | I32Eqz of Term
    | I32Add of lhs: Term * rhs: Term
    | I32Sub of lhs: Term * rhs: Term
    | I32Mul of lhs: Term * rhs: Term
    | If of cond: Term * thenClause: Term * elseClause: Term
    | Let of name: string * value: Term * body: Term
    | LetWithType of name: string * tyLit: TypeLiteral * value: Term * body: Term
    | Application of funcName: string * args: list<Term>
    | Var of name: string

type Context = list<string * Type>

type TypeEquation = Set<Type * Type>

type Assign = Assign of varName: string * ty: Type

let rec extract (context: Context) (term: Term) : Result<TypeEquation * Type, string> =
    match term with
    | I32Const _ -> Ok(Set.empty, I32)

    | I32Eqz t ->
        result {
            let! (equation, ty) = extract context t
            let equation = equation |> Set.add (ty, I32)
            return (equation, I32)
        }

    | I32Add (lhs, rhs)
    | I32Sub (lhs, rhs)
    | I32Mul (lhs, rhs) ->
        result {
            let! (e1, ty1) = extract context lhs
            let! (e2, ty2) = extract context rhs

            let e3 =
                Set.union e1 e2
                |> Set.add (ty1, I32)
                |> Set.add (ty2, I32)

            return (e3, I32)
        }

    | If (cond, thenClause, elseClause) ->
        result {
            let! (e1, _) = extract context cond
            let! (e2, ty2) = extract context thenClause
            let! (e3, ty3) = extract context elseClause

            let e =
                Set.union e1 e2
                |> Set.union e3
                |> Set.add (ty2, ty3)

            return (e, ty2)
        }

    | Let (name, value, body) ->
        result {
            let! (e1, ty1) = extract context value
            let! (e2, ty2) = extract ((name, ty1) :: context) body
            let e = Set.union e1 e2
            return (e, ty2)
        }

    | LetWithType (name, tyLit, value, body) ->
        result {
            let! (e1, ty1) = extract context value
            let! (e2, ty2) = extract ((name, ty1) :: context) body

            let e =
                Set.union e1 e2
                |> Set.add (ty1, Type.fromLiteral tyLit)

            return (e, ty2)
        }

    | Application (funcName, args) ->
        result {
            let! (_, funcType) =
                context
                |> List.tryFind (fun (name', _) -> name' = funcName)
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
                    let equation: TypeEquation =
                        (args |> List.map fst)
                        @ (args
                           |> List.mapi (fun i (_, ty) -> Set.singleton (ty, argTypes.[i])))
                        |> Set.unionMany

                    return (equation, retType)
            | _ -> return! Error "This is not a function"
        }

    | Var name ->
        result {
            let! (_, ty) =
                context
                |> List.tryFind (fun (name', _) -> name = name')
                |> Result.requireSome $"%s{name} is not defined"

            return (Set.empty, ty)
        }

let unify (equation: TypeEquation) : Result<list<Assign>, string> = failwith "not implemented"

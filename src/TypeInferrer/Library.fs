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

    override this.ToString() =
        match this with
        | I32 -> "i32"
        | I64 -> "i64"
        | F32 -> "f32"
        | F64 -> "f64"

        | TyVar name -> $"'%s{name}"

        | Func (args, ret) ->
            let args = args |> List.map string |> String.concat ", "
            $"(%s{args}) => %O{ret}"

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

type Context = private Context of list<string * Type>

module Context =
    let empty = Context []

    let add (name: string) (ty: Type) (Context context) = Context((name, ty) :: context)

    let tryFind (name: string) (Context context) : option<Type> =
        context
        |> List.tryFind (fun (name', ty) -> name' = name)
        |> Option.map snd

type TypeSimulEquation =
    private
    | TypeSimulEquation of Set<Type * Type>

    override this.ToString() =
        match this with
        | TypeSimulEquation equations ->
            equations
            |> Seq.map (fun (ty1, ty2) -> $"%O{ty1} = %O{ty2}")
            |> String.concat ", "
            |> sprintf "{ %s }"

module TypeSimulEquation =
    let empty = TypeSimulEquation Set.empty

    let addEquation (lhs: Type) (rhs: Type) (TypeSimulEquation equationSet) =
        TypeSimulEquation(Set.add (lhs, rhs) equationSet)

    let combine (TypeSimulEquation a) (TypeSimulEquation b) = TypeSimulEquation(Set.union a b)

    let combineMany (equations: seq<TypeSimulEquation>) =
        equations
        |> Seq.map (fun (TypeSimulEquation e) -> e)
        |> Set.unionMany
        |> TypeSimulEquation

type Assign = Assign of varName: string * ty: Type

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

let rec unify (equation: list<Type * Type>) : Result<list<Assign>, string> =
    match equation with
    | [] -> Ok []
    | (ty1, ty2) :: rest when ty1 = ty2 -> unify rest
    | ((TyVar name, ty)
    | (ty, TyVar name)) :: rest -> failwith "not implemented"
    | _ -> failwith "not implemented"

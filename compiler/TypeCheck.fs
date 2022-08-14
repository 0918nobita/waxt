[<AutoOpen>]
module Waxt.Compiler.TypeCheck

open FsToolkit.ErrorHandling

type TypeError = TypeError of msg: string * at: Range

module TypeEnv =
    type T = private TypeEnv of list<string * Type.T>

    let empty = TypeEnv []

    let add (name: string) (ty: Type.T) (TypeEnv typeEnv) = TypeEnv((name, ty) :: typeEnv)

    let find (name: string) (TypeEnv typeEnv) =
        typeEnv
        |> List.tryFind (fun (name', _) -> name = name')
        |> Option.map snd

let expectType (expected: Type.T) (actual: TypedExpr.T) =
    let range = (actual :> ILocatable).Locate()
    let actual = TypedExpr.getType actual

    if expected = actual then
        Ok()
    else
        let expected = Type.toString expected
        let actual = Type.toString actual
        Error(TypeError($"Type mismatch, expected: `%s{expected}`, actual: `%s{actual}`", range))

let rec checkType (typeEnv: TypeEnv.T) : Expr.T -> Result<TypedExpr.T, TypeError> =
    function
    | Expr.I32Add (lhs, rhs, at) ->
        result {
            let! lhs = checkType typeEnv lhs
            do! expectType Type.I32 lhs
            let! rhs = checkType typeEnv rhs
            do! expectType Type.I32 rhs
            return TypedExpr.I32Add(lhs, rhs, at)
        }

    | Expr.I32Const (value, at) -> Ok(TypedExpr.I32Const(value, at))

    | Expr.I32Mul (lhs, rhs, at) ->
        result {
            let! lhs = checkType typeEnv lhs
            do! expectType Type.I32 lhs
            let! rhs = checkType typeEnv rhs
            do! expectType Type.I32 rhs
            return TypedExpr.I32Mul(lhs, rhs, at)
        }

    | Expr.I32Store (addr, content, at) ->
        result {
            let! addr = checkType typeEnv addr
            do! expectType Type.I32 addr
            let! content = checkType typeEnv content
            do! expectType Type.I32 content
            return TypedExpr.I32Store(addr, content, at)
        }

    | Expr.Var (name, at) ->
        result {
            let! ty =
                TypeEnv.find name typeEnv
                |> Result.requireSome (TypeError($"{name} is not defined", at))

            return TypedExpr.Var(name, ty, at)
        }

// let checkFunc (result: Type.T) (parameters: list<string * Type.T>) (body: list<SExpr>) =
//     parameters
//     |> List.fold
//         (fun (names: Result<list<string>, TypeError>) (name, _) ->
//             match names with
//             | Ok names ->
//                 if List.contains name names
//                 then
//                     Error (TypeError("")))
//         (Ok [])
//     failwith "not implemented"

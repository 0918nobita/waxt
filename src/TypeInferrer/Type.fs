module Waxt.TypeInferrer.Type

open TypeLiteral

type Type =
    | I32
    | I64
    | F32
    | F64
    | TyVar of name: string
    | Func of FuncType

    override this.ToString() =
        match this with
        | I32 -> "i32"
        | I64 -> "i64"
        | F32 -> "f32"
        | F64 -> "f64"
        | TyVar name -> $"'%s{name}"
        | Func funcType -> $"%O{funcType}"

and FuncType =
    | FuncType of args: list<Type> * ret: Type

    override this.ToString() =
        match this with
        | FuncType (args, ret) ->
            let args = args |> List.map string |> String.concat ", "
            $"(%s{args}) => %O{ret}"

module Type =
    let fromLiteral (lit: TypeLiteral) =
        match lit with
        | I32TyLit -> I32
        | I64TyLit -> I64
        | F32TyLit -> F32
        | F64TyLit -> F64

    let rec freeTypeVars (ty: Type) =
        match ty with
        | TyVar name -> [ name ]
        | Func (FuncType (args, ret)) ->
            let args = args |> List.map freeTypeVars |> List.concat
            let ret = freeTypeVars ret
            (args @ ret) |> List.distinct
        | _ -> []

    let rec assign (tyVarName: string) (toTy: Type) (ty: Type) =
        match ty with
        | TyVar name when name = tyVarName -> toTy
        | Func (FuncType (args, ret)) ->
            let args = args |> List.map (assign tyVarName toTy)
            let ret = assign tyVarName toTy ret
            Func(FuncType(args, ret))
        | _ -> ty

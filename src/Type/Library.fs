namespace Waxt.Type

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

type TyVarName =
    private
    | TyVarName of string

    override this.ToString() =
        match this with
        | TyVarName name -> $"'%s{name}"

module TyVarName =
    let make name = TyVarName name

type Type =
    | Void
    | NumType of NumType
    | TyVar of TyVarName
    | Func of FuncType

    override this.ToString() =
        match this with
        | Void -> "void"
        | NumType numType -> $"%O{numType}"
        | TyVar name -> $"%O{name}"
        | Func funcType -> $"%O{funcType}"

and [<RequireQualifiedAccess>] NumType =
    | I32
    | I64
    | F32
    | F64

    override this.ToString() =
        match this with
        | I32 -> "i32"
        | I64 -> "i64"
        | F32 -> "f32"
        | F64 -> "f64"

and FuncType =
    | FuncType of args: list<Type> * ret: Type

    override this.ToString() =
        match this with
        | FuncType (args, ret) ->
            let args = args |> List.map string |> String.concat ", "
            $"(%s{args}) => %O{ret}"

module Type =
    let rec freeTypeVars (ty: Type) =
        match ty with
        | TyVar name -> [ name ]

        | Func (FuncType (args, ret)) ->
            let args = args |> List.map freeTypeVars |> List.concat
            let ret = freeTypeVars ret
            (args @ ret) |> List.distinct

        | _ -> []

    let rec assign (tyVarName: TyVarName) (toTy: Type) (ty: Type) =
        match ty with
        | TyVar name when name = tyVarName -> toTy

        | Func (FuncType (args, ret)) ->
            let args = args |> List.map (assign tyVarName toTy)
            let ret = assign tyVarName toTy ret
            Func(FuncType(args, ret))

        | _ -> ty

    let rec toJSON (ty: Type) =
        match ty with
        | Void -> Encode.string "voidType"
        | NumType numType ->
            Encode.object [ "type", Encode.string "numType"
                            "kind", Encode.string (string numType) ]
        | TyVar tyVarName ->
            Encode.object [ "type", Encode.string "tyVar"
                            "name", Encode.string (string tyVarName) ]
        | Func (FuncType (args, ret)) ->
            Encode.object [ "type", Encode.string "funcType"
                            "args", (args |> List.map toJSON |> Encode.list)
                            "ret", toJSON ret ]

[<AutoOpen>]
module Waxt.TypeInferrer.Type

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

namespace WAXT.AST

open Thoth.Json.Net

[<RequireQualifiedAccess>]
type TypeLiteral =
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

module TypeLiteral =
    let inline toJson (typeLiteral: TypeLiteral) = typeLiteral |> string |> Encode.string

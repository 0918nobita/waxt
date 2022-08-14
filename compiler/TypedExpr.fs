[<AutoOpen>]
module Waxt.Compiler.TypedExpr

open Thoth.Json.Net

module TypedExpr =
    type T =
        | I32Add of lhs: T * rhs: T * at: Range
        | I32Const of value: int * at: Range
        | I32Mul of lhs: T * rhs: T * at: Range
        | I32Store of addr: T * content: T * at: Range
        | Var of name: string * ty: Type.T * at: Range

        interface ILocatable with
            member this.Locate() =
                match this with
                | I32Add (_, _, at)
                | I32Const (_, at)
                | I32Mul (_, _, at)
                | I32Store (_, _, at)
                | Var (_, _, at) -> at

    let rec toJson: T -> JsonValue =
        function
        | I32Add (lhs, rhs, at) ->
            Encode.object [ "type", Encode.string "I32Add"
                            "lhs", toJson lhs
                            "rhs", toJson rhs
                            "at", Encode.string (Range.toString at) ]
        | I32Const (value, at) ->
            Encode.object [ "type", Encode.string "I32Const"
                            "value", Encode.int value
                            "at", Encode.string (Range.toString at) ]
        | I32Mul (lhs, rhs, at) ->
            Encode.object [ "type", Encode.string "I32Mul"
                            "lhs", toJson lhs
                            "rhs", toJson rhs
                            "at", Encode.string (Range.toString at) ]
        | I32Store (addr, content, at) ->
            Encode.object [ "type", Encode.string "I32Store"
                            "addr", toJson addr
                            "content", toJson content
                            "at", Encode.string (Range.toString at) ]
        | Var (name, ty, at) ->
            Encode.object [ "type", Encode.string "Var"
                            "name", Encode.string name
                            "ty", Type.toJson ty
                            "at", Encode.string (Range.toString at) ]

    let getType: T -> Type.T =
        function
        | I32Add _
        | I32Const _
        | I32Mul _ -> Type.I32
        | I32Store _ -> Type.Unit
        | Var (_, ty, _) -> ty

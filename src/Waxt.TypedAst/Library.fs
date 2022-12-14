[<AutoOpen>]
module Waxt.TypedAst.Library

open Thoth.Json.Net
open Waxt.Location
open Waxt.Type

type TypedExpr =
    | I32Add of lhs: TypedExpr * rhs: TypedExpr * at: Range
    | I32Const of value: int * at: Range
    | I32Mul of lhs: TypedExpr * rhs: TypedExpr * at: Range
    | I32Store of addr: TypedExpr * content: TypedExpr * at: Range
    | Var of index: int * ty: Type * at: Range

    interface ILocatable with
        member this.Locate() =
            match this with
            | I32Add (_, _, at)
            | I32Const (_, at)
            | I32Mul (_, _, at)
            | I32Store (_, _, at)
            | Var (_, _, at) -> at

module TypedExpr =
    let rec toJson: TypedExpr -> JsonValue =
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

        | Var (index, ty, at) ->
            Encode.object [ "type", Encode.string "Var"
                            "index", Encode.int index
                            "ty", Type.toJson ty
                            "at", Encode.string (Range.toString at) ]

    let getType: TypedExpr -> Type =
        function
        | I32Add _
        | I32Const _
        | I32Mul _ -> I32
        | I32Store _ -> Unit
        | Var (_, ty, _) -> ty

type FuncParams = IndexedMap<string, Range * (Type * Range)>

type FuncSig = FuncSig of parameters: FuncParams * result: Type * at: Range

type TypedFuncs = IndexedMap<string, FuncSig * list<TypedExpr>>

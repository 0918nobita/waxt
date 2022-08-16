[<AutoOpen>]
module Waxt.UntypedAst.Expr

open Thoth.Json.Net
open Waxt.Location

type Expr =
    | I32Add of lhs: Expr * rhs: Expr * at: Range
    | I32Const of value: int * at: Range
    | I32Mul of lhs: Expr * rhs: Expr * at: Range
    | I32Store of addr: Expr * content: Expr * at: Range
    | Var of name: string * at: Range

module Expr =
    let rec toJson: Expr -> JsonValue =
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
        | Var (name, at) ->
            Encode.object [ "type", Encode.string "Var"
                            "name", Encode.string name
                            "at", Encode.string (Range.toString at) ]

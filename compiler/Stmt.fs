[<AutoOpen>]
module Waxt.Compiler.Stmt

open Thoth.Json.Net

module Type =
    type T =
        | Unit
        | I32
        | F64

    let toString: T -> string =
        function
        | Unit -> "unit"
        | I32 -> "i32"
        | F64 -> "f64"

    let toJson: T -> JsonValue =
        function
        | Unit -> Encode.string "unit"
        | I32 -> Encode.string "i32"
        | F64 -> Encode.string "f64"

module Expr =
    type T =
        | I32Add of lhs: T * rhs: T * at: Range
        | I32Const of value: int * at: Range
        | I32Mul of lhs: T * rhs: T * at: Range
        | I32Store of addr: T * content: T * at: Range
        | Var of name: string * at: Range

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
        | Var (name, at) ->
            Encode.object [ "type", Encode.string "Var"
                            "name", Encode.string name
                            "at", Encode.string (Range.toString at) ]

type Stmt = FuncDef of name: string * result: Type.T * parameters: list<string * Type.T> * body: list<Expr.T>

module Stmt =
    let toJson: Stmt -> JsonValue =
        function
        | FuncDef (name, result, parameters, body) ->
            let parameters =
                parameters
                |> List.map (fun (name, ty) ->
                    Encode.object [ "name", Encode.string name
                                    "ty", Type.toJson ty ])
                |> Encode.list

            let body = body |> List.map Expr.toJson |> Encode.list

            Encode.object (
                [ "type", Encode.string "funcDef"
                  "name", Encode.string name
                  "result", Type.toJson result
                  "parameters", parameters
                  "body", body ]
            )

[<AutoOpen>]
module Waxt.Compiler.Stmt

open Thoth.Json.Net

type Ty = I32

module Ty =
    let toJson: Ty -> JsonValue =
        function
        | I32 -> Encode.string "i32"

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

type Stmt = FuncDef of name: string * result: option<Ty> * parameters: list<string * Ty> * body: list<Expr>

module Stmt =
    let toJson: Stmt -> JsonValue =
        function
        | FuncDef (name, result, parameters, body) ->
            let result =
                match result with
                | Some ty -> [ "result", Ty.toJson ty ]
                | None -> []

            let parameters =
                parameters
                |> List.map (fun (name, ty) ->
                    Encode.object [ "name", Encode.string name
                                    "ty", Ty.toJson ty ])
                |> Encode.list

            let body = body |> List.map Expr.toJson |> Encode.list

            Encode.object (
                [ "type", Encode.string "funcDef"
                  "name", Encode.string name ]
                @ result
                  @ [ "parameters", parameters
                      "body", body ]
            )

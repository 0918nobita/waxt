module Waxt.Ast.ExprExt

open Thoth.Json.Net
open Waxt.Location
open Waxt.Token
open Waxt.Type

open IfExprExt

let rec encodeExpr (expr: FixedExpr) =
    match expr with
    | I32Const (n, at) ->
        [ "type", Encode.string "i32Const"
          "value", Encode.int n
          "at", Range.toJSON at ]
        |> Encode.object

    | I32Eqz (arg, at) ->
        [ "type", Encode.string "i32Eqz"
          "arg", encodeExpr arg
          "at", Range.toJSON at ]
        |> Encode.object

    | I32Add (lhs, op, rhs) ->
        [ "type", Encode.string "i32Add"
          "lhs", encodeExpr lhs
          "opLoc", op |> I32AddOp.locate |> Range.toJSON
          "rhs", encodeExpr rhs ]
        |> Encode.object

    | I32Sub (lhs, op, rhs) ->
        [ "type", Encode.string "i32Add"
          "lhs", encodeExpr lhs
          "opLoc", op |> I32SubOp.locate |> Range.toJSON
          "rhs", encodeExpr rhs ]
        |> Encode.object

    | I32Mul (lhs, op, rhs) ->
        [ "type", Encode.string "i32Add"
          "lhs", encodeExpr lhs
          "opLoc", op |> I32MulOp.locate |> Range.toJSON
          "rhs", encodeExpr rhs ]
        |> Encode.object

    | If ifExpr -> encodeIfExpr encodeExpr ifExpr

    | Let (name, boundValue, body, at) ->
        [ "type", Encode.string "let"
          "name", VarName.toJSON name
          "boundValue", encodeExpr boundValue
          "body", encodeExpr body
          "at", Range.toJSON at ]
        |> Encode.object

    | LetWithType (name, ty, boundValue, body, at) ->
        [ "type", Encode.string "letWithType"
          "name", VarName.toJSON name
          "typeAnotation", TypeLiteral.toJSON ty
          "boundValue", encodeExpr boundValue
          "body", encodeExpr body
          "at", Range.toJSON at ]
        |> Encode.object

    | Application (funcName, args, at) ->
        [ "type", Encode.string "application"
          "funcName", FuncName.toJSON funcName
          "args",
          (args
           |> List.map (fun arg -> encodeExpr arg)
           |> Encode.list)
          "at", Range.toJSON at ]
        |> Encode.object

    | Var (varName, ty, at) ->
        [ "type", Encode.string "var"
          "varName", VarName.toJSON varName
          "varType", Type.toJSON ty
          "at", Range.toJSON at ]
        |> Encode.object

type Expr<'Ty> with
    member this.locate() =
        match this with
        | I32Const (_, at)
        | I32Eqz (_, at) -> at
        | I32Add (lhs, _, rhs)
        | I32Sub (lhs, _, rhs)
        | I32Mul (lhs, _, rhs) ->
            let lhs = lhs.locate ()
            let rhs = rhs.locate ()
            Range.combine lhs rhs
        | If ifExpr -> ifExpr.locate ()
        | Let (_, _, _, at)
        | LetWithType (_, _, _, _, at)
        | Application (_, _, at)
        | Var (_, _, at) -> at

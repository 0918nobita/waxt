namespace Waxt.Ast

open Thoth.Json.Net
open Waxt.Location
open Waxt.Token
open Waxt.Type

type Expr<'Ty> =
    | I32Const of n: int * at: Range

    | I32Eqz of Expr<'Ty> * at: Range

    | I32Add of lhs: Expr<'Ty> * op: I32AddOp * rhs: Expr<'Ty>
    | I32Sub of lhs: Expr<'Ty> * op: I32SubOp * rhs: Expr<'Ty>
    | I32Mul of lhs: Expr<'Ty> * op: I32MulOp * rhs: Expr<'Ty>

    | If of IfExpr<Expr<'Ty>>

    | Let of VarName * boundValue: Expr<'Ty> * body: Expr<'Ty> * at: Range
    | LetWithType of VarName * TypeLiteral * value: Expr<'Ty> * body: Expr<'Ty> * at: Range

    | Application of FuncName * args: list<Expr<'Ty>> * at: Range

    | Var of VarName * ty: 'Ty * at: Range

    interface IExpr with
        member this.locate() =
            match this with
            | I32Const (_, at)
            | I32Eqz (_, at) -> at
            | I32Add (lhs, _, rhs)
            | I32Sub (lhs, _, rhs)
            | I32Mul (lhs, _, rhs) ->
                let lhs = (lhs :> IExpr).locate ()
                let rhs = (rhs :> IExpr).locate ()
                Range.combine lhs rhs
            | If ifExpr -> IfExpr.locate ifExpr
            | Let (_, _, _, at)
            | LetWithType (_, _, _, _, at)
            | Application (_, _, at)
            | Var (_, _, at) -> at

type MutableExpr = Expr<ref<option<Type>>>

type MutableBlock = Block<MutableExpr>

type FixedExpr = Expr<Type>

type ExprEncoder =
    | ExprEncoder of FixedExpr

    interface IExprEncoder with
        member this.toJSON() =
            let (ExprEncoder expr) = this

            match expr with
            | I32Const (n, at) ->
                [ "type", Encode.string "i32Const"
                  "value", Encode.int n
                  "at", Range.toJSON at ]
                |> Encode.object

            | I32Eqz (arg, at) ->
                [ "type", Encode.string "i32Eqz"
                  "arg", (ExprEncoder arg :> IExprEncoder).toJSON ()
                  "at", Range.toJSON at ]
                |> Encode.object

            | I32Add (lhs, op, rhs) ->
                [ "type", Encode.string "i32Add"
                  "lhs", (ExprEncoder lhs :> IExprEncoder).toJSON ()
                  "opLoc", op |> I32AddOp.locate |> Range.toJSON
                  "rhs", (ExprEncoder rhs :> IExprEncoder).toJSON () ]
                |> Encode.object

            | I32Sub (lhs, op, rhs) ->
                [ "type", Encode.string "i32Add"
                  "lhs", (ExprEncoder lhs :> IExprEncoder).toJSON ()
                  "opLoc", op |> I32SubOp.locate |> Range.toJSON
                  "rhs", (ExprEncoder rhs :> IExprEncoder).toJSON () ]
                |> Encode.object

            | I32Mul (lhs, op, rhs) ->
                [ "type", Encode.string "i32Add"
                  "lhs", (ExprEncoder lhs :> IExprEncoder).toJSON ()
                  "opLoc", op |> I32MulOp.locate |> Range.toJSON
                  "rhs", (ExprEncoder rhs :> IExprEncoder).toJSON () ]
                |> Encode.object

            | If ifExpr -> IfExpr.toJSON (fun expr -> ExprEncoder expr) ifExpr

            | Let (name, boundValue, body, at) ->
                [ "type", Encode.string "let"
                  "name", VarName.toJSON name
                  "boundValue", (ExprEncoder boundValue :> IExprEncoder).toJSON ()
                  "body", (ExprEncoder body :> IExprEncoder).toJSON ()
                  "at", Range.toJSON at ]
                |> Encode.object

            | LetWithType (name, ty, boundValue, body, at) ->
                [ "type", Encode.string "letWithType"
                  "name", VarName.toJSON name
                  "typeAnotation", TypeLiteral.toJSON ty
                  "boundValue", (ExprEncoder boundValue :> IExprEncoder).toJSON ()
                  "body", (ExprEncoder body :> IExprEncoder).toJSON ()
                  "at", Range.toJSON at ]
                |> Encode.object

            | Application (funcName, args, at) ->
                [ "type", Encode.string "application"
                  "funcName", FuncName.toJSON funcName
                  "args",
                  (args
                   |> List.map (fun arg -> (ExprEncoder arg :> IExprEncoder).toJSON ())
                   |> Encode.list)
                  "at", Range.toJSON at ]
                |> Encode.object

            | Var (varName, ty, at) ->
                [ "type", Encode.string "var"
                  "varName", VarName.toJSON varName
                  "varType", Type.toJSON ty
                  "at", Range.toJSON at ]
                |> Encode.object

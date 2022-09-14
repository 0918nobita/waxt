namespace Waxt.Ast

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif
open Waxt.Location
open Waxt.Token
open Waxt.Type

type Expr<'Ty> =
    | I32Const of n: int * at: Range

    | I32Eqz of Parenthesized<{| Ident: Ident; Arg: Expr<'Ty> |}>

    | I32Add of
        Parenthesized<{| Ident: Ident
                         Lhs: Expr<'Ty>
                         Rhs: Expr<'Ty> |}>

    | I32Sub of
        Parenthesized<{| Ident: Ident
                         Lhs: Expr<'Ty>
                         Rhs: Expr<'Ty> |}>

    | I32Mul of
        Parenthesized<{| Ident: Ident
                         Lhs: Expr<'Ty>
                         Rhs: Expr<'Ty> |}>

    | If of IfExpr<Expr<'Ty>>

    | Let of
        Parenthesized<{| VarName: Ident
                         BoundValue: Expr<'Ty>
                         Body: Expr<'Ty> |}>

    | LetWithType of
        Parenthesized<{| VarName: Ident
                         TypeAnnotation: TypeLiteral
                         BoundValue: Expr<'Ty>
                         Body: Expr<'Ty> |}>

    | Application of
        Parenthesized<{| FuncName: Ident
                         Args: list<Expr<'Ty>> |}>

    | Var of varName: Ident * ty: 'Ty

    interface IExpr with
        member this.locate() =
            match this with
            | I32Const (_, at) -> at
            | I32Eqz parenthesized -> Parenthesized.locate parenthesized
            | I32Add parenthesized
            | I32Sub parenthesized
            | I32Mul parenthesized -> Parenthesized.locate parenthesized
            | If ifExpr -> IfExpr.locate ifExpr
            | Let parenthesized -> Parenthesized.locate parenthesized
            | LetWithType parenthesized -> Parenthesized.locate parenthesized
            | Application parenthesized -> Parenthesized.locate parenthesized
            | Var (varName, _) -> Ident.locate varName

type MutableExpr = Expr<ref<option<Type>>>

type MutableBlock = Block<MutableExpr>

type FixedExpr = Expr<Type>

module FixedExpr =
    let rec encodeExpr (expr: FixedExpr) =
        match expr with
        | I32Const (n, at) ->
            [ "type", Encode.string "i32Const"
              "value", Encode.int n
              "at", Range.toJSON at ]
            |> Encode.object

        | I32Eqz (Parenthesized inner) ->
            [ "type", Encode.string "i32Eqz"
              "arg", encodeExpr inner.Arg ]
            |> Encode.object

        | I32Add (Parenthesized inner) ->
            [ "type", Encode.string "i32Add"
              "ident", Ident.toJSON inner.Ident
              "lhs", encodeExpr inner.Lhs
              "rhs", encodeExpr inner.Rhs ]
            |> Encode.object

        | I32Sub (Parenthesized inner) ->
            [ "type", Encode.string "i32Sub"
              "ident", Ident.toJSON inner.Ident
              "lhs", encodeExpr inner.Lhs
              "rhs", encodeExpr inner.Rhs ]
            |> Encode.object

        | I32Mul (Parenthesized inner) ->
            [ "type", Encode.string "i32Mul"
              "ident", Ident.toJSON inner.Ident
              "lhs", encodeExpr inner.Lhs
              "rhs", encodeExpr inner.Rhs ]
            |> Encode.object

        | If ifExpr -> IfExpr.toJSON (fun expr -> encodeExpr expr) ifExpr

        | Let (Parenthesized inner) ->
            [ "type", Encode.string "let"
              "name", Ident.toJSON inner.VarName
              "boundValue", encodeExpr inner.BoundValue
              "body", encodeExpr inner.Body ]
            |> Encode.object

        | LetWithType (Parenthesized inner) ->
            [ "type", Encode.string "letWithType"
              "name", Ident.toJSON inner.VarName
              "typeAnotation", TypeLiteral.toJSON inner.TypeAnnotation
              "boundValue", encodeExpr inner.BoundValue
              "body", encodeExpr inner.Body ]
            |> Encode.object

        | Application (Parenthesized inner) ->
            [ "type", Encode.string "application"
              "funcName", Ident.toJSON inner.FuncName
              "args", (inner.Args |> List.map encodeExpr |> Encode.list) ]
            |> Encode.object

        | Var (varName, ty) ->
            [ "type", Encode.string "var"
              "varName", Ident.toJSON varName
              "varType", Type.toJSON ty ]
            |> Encode.object

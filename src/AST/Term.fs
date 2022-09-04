namespace WAXT.AST

open Thoth.Json.Net
open WAXT.Location
open WAXT.Token
open WAXT.Type

type Term<'Ty> =
    | I32Const of n: int * at: Range

    | I32Eqz of Term<'Ty> * at: Range

    | I32Add of lhs: Term<'Ty> * op: I32AddOp * rhs: Term<'Ty>
    | I32Sub of lhs: Term<'Ty> * op: I32SubOp * rhs: Term<'Ty>
    | I32Mul of lhs: Term<'Ty> * op: I32MulOp * rhs: Term<'Ty>

    | If of IfExpr<'Ty>

    | Let of VarName * boundValue: Term<'Ty> * body: Term<'Ty> * at: Range
    | LetWithType of VarName * TypeLiteral * value: Term<'Ty> * body: Term<'Ty> * at: Range

    | Application of FuncName * args: list<Term<'Ty>> * at: Range

    | Var of VarName * ty: 'Ty * at: Range

and IfExpr<'Ty> =
    | IfExpr of
        if_: IfKeyword *
        cond: Term<'Ty> *
        thenClause: {| OpenBrace: OpenBrace
                       Body: Term<'Ty>
                       CloseBrace: CloseBrace |} *
        elseClause: {| OpenBrace: OpenBrace
                       Body: Term<'Ty>
                       CloseBrace: CloseBrace |}

type MutableTerm = Term<ref<option<Type>>>

type DereferencedTerm = Term<Type>

module DereferencedTerm =
    let rec toJSON (term: DereferencedTerm) : JsonValue =
        match term with
        | I32Const (n, at) ->
            Encode.object [ "type", Encode.string "i32Const"
                            "value", Encode.int n
                            "at", Range.toJSON at ]

        | I32Eqz (term, at) ->
            Encode.object [ "type", Encode.string "i32Eqz"
                            "term", toJSON term
                            "at", Range.toJSON at ]

        | I32Add (lhs, op, rhs) ->
            Encode.object [ "type", Encode.string "i32Add"
                            "lhs", toJSON lhs
                            "op", (op :> ILocatable).Locate() |> Range.toJSON
                            "rhs", toJSON rhs ]

        | I32Sub (lhs, op, rhs) ->
            Encode.object [ "type", Encode.string "i32Sub"
                            "lhs", toJSON lhs
                            "op", (op :> ILocatable).Locate() |> Range.toJSON
                            "rhs", toJSON rhs ]

        | I32Mul (lhs, op, rhs) ->
            Encode.object [ "type", Encode.string "i32Mul"
                            "lhs", toJSON lhs
                            "op", (op :> ILocatable).Locate() |> Range.toJSON
                            "rhs", toJSON rhs ]

        | If ifExpr -> ifExprToJSON ifExpr

        | Let (name, boundValue, body, at) ->
            Encode.object [ "type", Encode.string "let"
                            "name", VarName.toJSON name
                            "boundValue", toJSON boundValue
                            "body", toJSON body
                            "at", Range.toJSON at ]

        | LetWithType (name, ty, boundValue, body, at) ->
            Encode.object [ "type", Encode.string "letWithType"
                            "name", VarName.toJSON name
                            "typeAnotation", TypeLiteral.toJSON ty
                            "boundValue", toJSON boundValue
                            "body", toJSON body
                            "at", Range.toJSON at ]

        | Application (funcName, args, at) ->
            Encode.object [ "type", Encode.string "application"
                            "funcName", FuncName.toJSON funcName
                            "args", (args |> List.map toJSON |> Encode.list)
                            "at", Range.toJSON at ]

        | Var (varName, ty, at) ->
            Encode.object [ "type", Encode.string "var"
                            "varName", VarName.toJSON varName
                            "varType", Type.toJSON ty
                            "at", Range.toJSON at ]

    and private ifExprToJSON (IfExpr (if_, cond, thenClause, elseClause)) =
        let thenClause =
            Encode.object [ "openBrace",
                            (thenClause.OpenBrace :> ILocatable).Locate()
                            |> Range.toJSON
                            "body", toJSON thenClause.Body
                            "closeBrace",
                            (thenClause.CloseBrace :> ILocatable).Locate()
                            |> Range.toJSON ]

        let elseClause =
            Encode.object [ "openBrace",
                            (elseClause.OpenBrace :> ILocatable).Locate()
                            |> Range.toJSON
                            "body", toJSON elseClause.Body
                            "closeBrace",
                            (elseClause.CloseBrace :> ILocatable).Locate()
                            |> Range.toJSON ]

        Encode.object [ "type", Encode.string "if"
                        "if", (if_ :> ILocatable).Locate() |> Range.toJSON
                        "cond", toJSON cond
                        "then", thenClause
                        "else", elseClause ]

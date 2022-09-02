namespace WAXT.AST

open Thoth.Json.Net
open WAXT.Location
open WAXT.Type

type Term<'Ty> =
    | I32Const of n: int * at: Range
    | I32Eqz of Term<'Ty> * at: Range
    | I32Add of lhs: Term<'Ty> * rhs: Term<'Ty> * at: Range
    | I32Sub of lhs: Term<'Ty> * rhs: Term<'Ty> * at: Range
    | I32Mul of lhs: Term<'Ty> * rhs: Term<'Ty> * at: Range
    | If of cond: Term<'Ty> * thenClause: Term<'Ty> * elseClause: Term<'Ty> * at: Range
    | Let of VarName * boundValue: Term<'Ty> * body: Term<'Ty> * at: Range
    | LetWithType of VarName * TypeLiteral * value: Term<'Ty> * body: Term<'Ty> * at: Range
    | Application of FuncName * args: list<Term<'Ty>> * at: Range
    | Var of VarName * ty: 'Ty * at: Range

type MutableTerm = Term<ref<option<Type>>>

type DereferencedTerm = Term<Type>

module DereferencedTerm =
    let rec toJson (term: DereferencedTerm) : JsonValue =
        match term with
        | I32Const (n, at) ->
            Encode.object [ "type", Encode.string "i32Const"
                            "value", Encode.int n
                            "at", Range.toJson at ]

        | I32Eqz (term, at) ->
            Encode.object [ "type", Encode.string "i32Eqz"
                            "term", toJson term
                            "at", Range.toJson at ]

        | I32Add (lhs, rhs, at) ->
            Encode.object [ "type", Encode.string "i32Add"
                            "lhs", toJson lhs
                            "rhs", toJson rhs
                            "at", Range.toJson at ]

        | I32Sub (lhs, rhs, at) ->
            Encode.object [ "type", Encode.string "i32Sub"
                            "lhs", toJson lhs
                            "rhs", toJson rhs
                            "at", Range.toJson at ]

        | I32Mul (lhs, rhs, at) ->
            Encode.object [ "type", Encode.string "i32Mul"
                            "lhs", toJson lhs
                            "rhs", toJson rhs
                            "at", Range.toJson at ]

        | If (cond, thenClause, elseClause, at) ->
            Encode.object [ "type", Encode.string "if"
                            "cond", toJson cond
                            "then", toJson thenClause
                            "else", toJson elseClause
                            "at", Range.toJson at ]

        | Let (name, boundValue, body, at) ->
            Encode.object [ "type", Encode.string "let"
                            "name", VarName.toJson name
                            "boundValue", toJson boundValue
                            "body", toJson body
                            "at", Range.toJson at ]

        | LetWithType (name, ty, boundValue, body, at) ->
            Encode.object [ "type", Encode.string "letWithType"
                            "name", VarName.toJson name
                            "typeAnotation", TypeLiteral.toJson ty
                            "boundValue", toJson boundValue
                            "body", toJson body
                            "at", Range.toJson at ]

        | Application (funcName, args, at) ->
            Encode.object [ "type", Encode.string "application"
                            "funcName", FuncName.toJson funcName
                            "args", (args |> List.map toJson |> Encode.list)
                            "at", Range.toJson at ]

        | Var (varName, ty, at) ->
            Encode.object [ "type", Encode.string "var"
                            "varName", VarName.toJson varName
                            "varType", Type.toJson ty
                            "at", Range.toJson at ]

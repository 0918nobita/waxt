namespace WAXT.AST

open System.Runtime.CompilerServices
open Thoth.Json.Net
open WAXT.Location
open WAXT.Token
open WAXT.Type

[<Extension>]
type TermExtensions =
    [<Extension>]
    static member toJSON(term: DereferencedTerm) =
        Encode.object
        <| match term with
           | I32Const (n, at) ->
               [ "type", Encode.string "i32Const"
                 "value", Encode.int n
                 "at", Range.toJSON at ]

           | I32Eqz (term, at) ->
               [ "type", Encode.string "i32Eqz"
                 "term", term.toJSON ()
                 "at", Range.toJSON at ]

           | I32Add (lhs, op, rhs) ->
               [ "type", Encode.string "i32Add"
                 "lhs", lhs.toJSON ()
                 "op", op |> I32AddOp.locate |> Range.toJSON
                 "rhs", rhs.toJSON () ]

           | I32Sub (lhs, op, rhs) ->
               [ "type", Encode.string "i32Sub"
                 "lhs", lhs.toJSON ()
                 "op", op |> I32SubOp.locate |> Range.toJSON
                 "rhs", rhs.toJSON () ]

           | I32Mul (lhs, op, rhs) ->
               [ "type", Encode.string "i32Mul"
                 "lhs", lhs.toJSON ()
                 "op", op |> I32MulOp.locate |> Range.toJSON
                 "rhs", rhs.toJSON () ]

           | If ifExpr -> ifExpr.toJSON ()

           | Let (name, boundValue, body, at) ->
               [ "type", Encode.string "let"
                 "name", VarName.toJSON name
                 "boundValue", boundValue.toJSON ()
                 "body", body.toJSON ()
                 "at", Range.toJSON at ]

           | LetWithType (name, ty, boundValue, body, at) ->
               [ "type", Encode.string "letWithType"
                 "name", VarName.toJSON name
                 "typeAnotation", TypeLiteral.toJSON ty
                 "boundValue", boundValue.toJSON ()
                 "body", body.toJSON ()
                 "at", Range.toJSON at ]

           | Application (funcName, args, at) ->
               [ "type", Encode.string "application"
                 "funcName", FuncName.toJSON funcName
                 "args",
                 (args
                  |> List.map (fun arg -> arg.toJSON ())
                  |> Encode.list)
                 "at", Range.toJSON at ]

           | Var (varName, ty, at) ->
               [ "type", Encode.string "var"
                 "varName", VarName.toJSON varName
                 "varType", Type.toJSON ty
                 "at", Range.toJSON at ]

    [<Extension>]
    static member toJSON((IfExpr (if_, cond, thenClause, elseClause)): IfExpr<Type>) =
        let thenClause =
            [ "openBrace",
              thenClause.OpenBrace
              |> OpenBrace.locate
              |> Range.toJSON

              "body", thenClause.Body.toJSON ()

              "closeBrace",
              thenClause.CloseBrace
              |> CloseBrace.locate
              |> Range.toJSON ]
            |> Encode.object

        let elseClause =
            [ "openBrace",
              elseClause.OpenBrace
              |> OpenBrace.locate
              |> Range.toJSON

              "body", elseClause.Body.toJSON ()

              "closeBrace",
              elseClause.CloseBrace
              |> CloseBrace.locate
              |> Range.toJSON ]
            |> Encode.object

        [ "type", Encode.string "if"
          "if", if_ |> IfKeyword.locate |> Range.toJSON
          "cond", cond.toJSON ()
          "then", thenClause
          "else", elseClause ]

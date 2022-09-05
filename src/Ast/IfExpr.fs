module Waxt.Ast.IfExprExt

open Thoth.Json.Net
open Waxt.Location
open Waxt.Token

type IfExpr<'Ty> with
    member this.locate() =
        let (IfExpr (if_, _, _, Block (_, _, closeBrace))) = this
        let if_ = IfKeyword.locate if_
        let closeBrace = CloseBrace.locate closeBrace
        Range.combine if_ closeBrace

let encodeIfExpr (encodeExpr: FixedExpr -> JsonValue) ((IfExpr (if_, cond, thenClause, elseClause)): FixedIfExpr) =
    let thenClause = Block.toJSON encodeExpr thenClause

    let elseClause = Block.toJSON encodeExpr elseClause

    [ "type", Encode.string "if"
      "if", if_ |> IfKeyword.locate |> Range.toJSON
      "cond", encodeExpr cond
      "then", thenClause
      "else", elseClause ]
    |> Encode.object

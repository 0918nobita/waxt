namespace Waxt.Ast

open Thoth.Json.Net
open Waxt.Location

type IExpr =
    abstract member locate: unit -> Range

type EncodeExpr<'Expr when 'Expr :> IExpr> = 'Expr -> JsonValue

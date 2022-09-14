namespace Waxt.Ast

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif
open Waxt.Location

type IExpr =
    abstract member locate: unit -> Range

type EncodeExpr<'Expr when 'Expr :> IExpr> = 'Expr -> JsonValue

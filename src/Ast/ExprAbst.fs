namespace Waxt.Ast

open Thoth.Json.Net
open Waxt.Location

type IExpr =
    abstract member locate: unit -> Range

type IExprEncoder =
    abstract member toJSON: unit -> JsonValue

module Waxt.Ast.Block

open Thoth.Json.Net
open Waxt.Location
open Waxt.Token

let toJSON (encodeExpr: FixedExpr -> JsonValue) (Block (openBrace, (body, last), closeBrace)) =
    let body =
        body @ [ last ]
        |> List.map encodeExpr
        |> Encode.list

    [ "type", Encode.string "block"
      "openBrace", openBrace |> OpenBrace.locate |> Range.toJSON
      "body", body
      "closeBrace", closeBrace |> CloseBrace.locate |> Range.toJSON ]
    |> Encode.object

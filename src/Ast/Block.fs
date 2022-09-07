namespace Waxt.Ast

open Thoth.Json.Net
open Waxt.Location
open Waxt.Token

type Block<'Expr when 'Expr :> IExpr> =
    private | Block of openBrace: OpenBrace * body: (list<'Expr> * 'Expr) * closeBrace: CloseBrace

module Block =
    let make openBrace preceding last closeBrace =
        Block(openBrace, (preceding, last), closeBrace)

    let openBrace (Block (openBrace, _, _)) = openBrace

    let precedingBody (Block (_, (preceding, _), _)) = preceding

    let lastBody (Block (_, (_, last), _)) = last

    let closeBrace (Block (_, _, closeBrace)) = closeBrace

    let locate (Block (openBrace, _, closeBrace)) =
        let openBrace = OpenBrace.locate openBrace
        let closeBrace = CloseBrace.locate closeBrace
        Range.combine openBrace closeBrace

    let toJSON (encodeExpr: EncodeExpr<'Expr>) (block: Block<'Expr>) =
        let (Block (openBrace, (preceding, last), closeBrace)) = block

        let body =
            preceding @ [ last ]
            |> List.map encodeExpr
            |> Encode.list

        [ "type", Encode.string "block"
          "openBrace", openBrace |> OpenBrace.locate |> Range.toJSON
          "body", body
          "closeBrace", closeBrace |> CloseBrace.locate |> Range.toJSON ]
        |> Encode.object

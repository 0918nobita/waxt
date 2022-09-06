namespace Waxt.Ast

open Thoth.Json.Net
open Waxt.Location
open Waxt.Token

type Block<'Expr when 'Expr :> IExpr> =
    private | Block of openBrace: OpenBrace * body: (list<'Expr> * 'Expr) * closeBrace: CloseBrace

module Block =
    let make openBrace body closeBrace = Block(openBrace, body, closeBrace)

    let openBrace (Block (openBrace, _, _)) = openBrace

    let body (Block (_, body, _)) = body

    let closeBrace (Block (_, _, closeBrace)) = closeBrace

    let locate (Block (openBrace, _, closeBrace)) =
        let openBrace = OpenBrace.locate openBrace
        let closeBrace = CloseBrace.locate closeBrace
        Range.combine openBrace closeBrace

    let toJSON (createExprEncoder: 'Expr -> IExprEncoder) (block: Block<'Expr>) =
        let (Block (openBrace, (body, last), closeBrace)) = block

        let body =
            body @ [ last ]
            |> List.map (fun expr -> (createExprEncoder expr).toJSON ())
            |> Encode.list

        [ "type", Encode.string "block"
          "openBrace", openBrace |> OpenBrace.locate |> Range.toJSON
          "body", body
          "closeBrace", closeBrace |> CloseBrace.locate |> Range.toJSON ]
        |> Encode.object

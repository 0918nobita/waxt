namespace Waxt.Ast

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif
open Waxt.Location
open Waxt.Token

type Block<'Expr when 'Expr :> IExpr> =
    private | Block of openParen: LeftParen * body: list<'Expr> * closeParen: RightParen

module Block =
    let make openParen body closeParen = Block(openParen, body, closeParen)

    let openParen (Block (openParen, _, _)) = openParen

    let body (Block (_, body, _)) = body

    let closeParen (Block (_, _, closeParen)) = closeParen

    let locate (Block (openParen, _, closeParen)) =
        let openBrace = LeftParen.locate openParen
        let closeBrace = RightParen.locate closeParen
        Range.combine openBrace closeBrace

    let toJSON (encodeExpr: EncodeExpr<'Expr>) (block: Block<'Expr>) =
        let (Block (openParen, body, closeParen)) = block

        let body = body |> List.map encodeExpr |> Encode.list

        [ "type", Encode.string "block"
          "openParen", openParen |> LeftParen.locate |> Range.toJSON
          "body", body
          "closeParen", closeParen |> RightParen.locate |> Range.toJSON ]
        |> Encode.object

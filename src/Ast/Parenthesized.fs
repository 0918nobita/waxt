namespace Waxt.Ast

open Thoth.Json.Net
open Waxt.Location
open Waxt.Token

type Parenthesized<'Inner> = private ParenthesizedC of openParen: LeftParen * inner: 'Inner * closeParen: RightParen

[<AutoOpen>]
module ParenthesizedActivePattterns =
    let (|Parenthesized|) (ParenthesizedC (_, inner, _)) = Parenthesized inner

module Parenthesized =
    let make openParen inner closeParen =
        ParenthesizedC(openParen, inner, closeParen)

    let setInner inner' (ParenthesizedC (openParen, _, closeParen)) =
        ParenthesizedC(openParen, inner', closeParen)

    let locate (ParenthesizedC (openParen, _, closeParen)) =
        let openParen = LeftParen.locate openParen
        let closeParen = RightParen.locate closeParen
        Range.combine openParen closeParen

    let toJSON (encodeExpr: EncodeExpr<'Expr>) (parenthesized: Parenthesized<'Expr>) =
        let (ParenthesizedC (openParen, inner, closeParen)) = parenthesized

        [ "openParen", LeftParen.toJSON openParen
          "inner", encodeExpr inner
          "closeParen", RightParen.toJSON closeParen ]
        |> Encode.object

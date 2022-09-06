namespace Waxt.Ast

open Thoth.Json.Net
open Waxt.Location
open Waxt.Token

type IfExpr<'Expr when 'Expr :> IExpr> =
    private | IfExpr of ifKeyword: IfKeyword * cond: 'Expr * thenClause: Block<'Expr> * elseClause: Block<'Expr>

module IfExpr =
    let make ifKeyword cond thenClause elseClause =
        IfExpr(ifKeyword, cond, thenClause, elseClause)

    let ifKeyword (IfExpr (ifKeyword, _, _, _)) = ifKeyword

    let cond (IfExpr (_, cond, _, _)) = cond

    let thenClause (IfExpr (_, _, thenClause, _)) = thenClause

    let elseClause (IfExpr (_, _, _, elseClause)) = elseClause

    let locate (IfExpr (ifKeyword, _, _, elseClause)) =
        let ifKeyword = IfKeyword.locate ifKeyword
        let elseClause = Block.locate elseClause
        Range.combine ifKeyword elseClause

    let toJSON (createExprEncoder: 'Expr -> IExprEncoder) (ifExpr: IfExpr<'Expr>) =
        let (IfExpr (ifKeyword, cond, thenClause, elseClause)) = ifExpr
        let thenClause = Block.toJSON createExprEncoder thenClause

        let elseClause = Block.toJSON createExprEncoder elseClause

        [ "type", Encode.string "if"
          "if", ifKeyword |> IfKeyword.locate |> Range.toJSON
          "cond", (createExprEncoder cond).toJSON ()
          "then", thenClause
          "else", elseClause ]
        |> Encode.object

namespace Waxt.Ast

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif
open Waxt.Location
open Waxt.Token

type IfExpr<'Expr when 'Expr :> IExpr> =
    private | IfExpr of ifIdent: Ident * cond: 'Expr * thenClause: Block<'Expr> * elseClause: Block<'Expr>

module IfExpr =
    let make ifIdent cond thenClause elseClause =
        IfExpr(ifIdent, cond, thenClause, elseClause)

    let ifIdent (IfExpr (ifIdent, _, _, _)) = ifIdent

    let cond (IfExpr (_, cond, _, _)) = cond

    let thenClause (IfExpr (_, _, thenClause, _)) = thenClause

    let elseClause (IfExpr (_, _, _, elseClause)) = elseClause

    let locate (IfExpr (ifIdent, _, _, elseClause)) =
        let ifKeyword = Ident.locate ifIdent
        let elseClause = Block.locate elseClause
        Range.combine ifKeyword elseClause

    let toJSON (encodeExpr: EncodeExpr<'Expr>) (ifExpr: IfExpr<'Expr>) =
        let (IfExpr (ifIdent, cond, thenClause, elseClause)) = ifExpr
        let thenClause = Block.toJSON encodeExpr thenClause

        let elseClause = Block.toJSON encodeExpr elseClause

        [ "type", Encode.string "if"
          "if", ifIdent |> Ident.locate |> Range.toJSON
          "cond", encodeExpr cond
          "then", thenClause
          "else", elseClause ]
        |> Encode.object

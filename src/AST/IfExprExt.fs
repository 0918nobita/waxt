module WAXT.AST.IfExprExt

open WAXT.Location
open WAXT.Token

type IfExpr<'Ty> with
    member this.locate() =
        match this with
        | IfExpr (if_, _, _, elseClause) ->
            let if_ = IfKeyword.locate if_
            let closeBrace = CloseBrace.locate elseClause.CloseBrace
            Range.combine if_ closeBrace

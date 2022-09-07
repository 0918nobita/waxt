module Waxt.TypeInferrer.DerefType

open FsToolkit.ErrorHandling
open Waxt.Ast
open Waxt.Token
open Waxt.Type

let rec derefType (assigns: list<Assign>) (expr: MutableExpr) : Result<FixedExpr, list<TypeInferenceError>> =
    match expr with
    | I32Const (n, at) -> Ok(I32Const(n, at))

    | I32Eqz (Parenthesized inner as parenthesized) ->
        result {
            let! arg = derefType assigns inner.Arg

            return
                I32Eqz(
                    parenthesized
                    |> Parenthesized.setInner {| inner with Arg = arg |}
                )
        }

    | I32Add (Parenthesized inner as parenthesized) ->
        result {
            let! lhs = derefType assigns inner.Lhs
            let! rhs = derefType assigns inner.Rhs

            return
                I32Add(
                    parenthesized
                    |> Parenthesized.setInner {| inner with Lhs = lhs; Rhs = rhs |}
                )
        }

    | I32Sub (Parenthesized inner as parenthesized) ->
        result {
            let! lhs = derefType assigns inner.Lhs
            let! rhs = derefType assigns inner.Rhs

            return
                I32Sub(
                    parenthesized
                    |> Parenthesized.setInner {| inner with Lhs = lhs; Rhs = rhs |}
                )
        }

    | I32Mul (Parenthesized inner as parenthesized) ->
        result {
            let! lhs = derefType assigns inner.Lhs
            let! rhs = derefType assigns inner.Rhs

            return
                I32Mul(
                    parenthesized
                    |> Parenthesized.setInner {| inner with Lhs = lhs; Rhs = rhs |}
                )
        }

    | If ifExpr ->
        result {
            let! cond = derefType assigns (IfExpr.cond ifExpr)
            let! thenClause = derefTypeInBlock assigns (IfExpr.thenClause ifExpr)
            let! elseClause = derefTypeInBlock assigns (IfExpr.elseClause ifExpr)

            return If(IfExpr.make (IfExpr.ifIdent ifExpr) cond thenClause elseClause)
        }

    | Let (Parenthesized inner as parenthesized) ->
        result {
            let! boundValue = derefType assigns inner.BoundValue
            let! body = derefType assigns inner.Body

            return
                Let(
                    parenthesized
                    |> Parenthesized.setInner
                        {| inner with
                            BoundValue = boundValue
                            Body = body |}
                )
        }

    | LetWithType (Parenthesized inner as parenthesized) ->
        result {
            let! boundValue = derefType assigns inner.BoundValue
            let! body = derefType assigns inner.Body

            return
                LetWithType(
                    parenthesized
                    |> Parenthesized.setInner
                        {| inner with
                            BoundValue = boundValue
                            Body = body |}
                )
        }

    | Application (Parenthesized inner as parenthesized) ->
        result {
            let! args =
                inner.Args
                |> List.map (derefType assigns)
                |> List.sequenceResultA
                |> Result.mapError (fun err -> List.concat err)

            return
                Application(
                    parenthesized
                    |> Parenthesized.setInner {| inner with Args = args |}
                )
        }

    | Var (varName, tyRef) ->
        let at = Ident.locate varName

        result {
            let! ty =
                tyRef.Value
                |> Result.requireSome [ TypeInferenceError("(Fatal) Failed to get type information", at) ]

            let! ty =
                match ty with
                | TyVar tyVarName ->
                    assigns
                    |> List.tryFind (fun (Assign (tyVarName', _)) -> tyVarName = tyVarName')
                    |> Option.map (fun (Assign (_, ty')) -> ty')
                    |> Result.requireSome [ TypeInferenceError($"%O{tyVarName} was not resolved", at) ]
                | _ -> Ok ty

            return Var(varName, ty)
        }

and private derefTypeInBlock (assigns: list<Assign>) (block: MutableBlock) =
    result {
        let! body =
            block
            |> Block.body
            |> List.map (derefType assigns)
            |> List.sequenceResultA
            |> Result.mapError List.concat

        return Block.make (Block.openParen block) body (Block.closeParen block)
    }

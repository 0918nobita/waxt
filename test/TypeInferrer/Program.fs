module Waxt.TypeInferrer.Test.Program

open NUnit.Framework
open Waxt.Ast
open Waxt.Ast.ExprExt
open Waxt.Location
open Waxt.TestUtil
open Waxt.Token
open Waxt.Type
open Waxt.TypeInferrer
open Waxt.TypeInferrer.DerefType
open Waxt.TypeInferrer.Extract
open Waxt.TypeInferrer.Unify

[<Test>]
let Test1 () =
    let pos = Pos.origin
    let at = Range.fromPos pos

    let fact = FuncName.make "fact"
    let n = VarName.make "n"

    let t0 = TyVar(TyVarName.make "t0")
    let t1 = TyVar(TyVarName.make "t1")

    let funcContext =
        FuncContext.empty
        |> FuncContext.add fact (FuncType([ t0 ], t1))

    let varContext = VarContext.empty |> VarContext.add n t0

    let expr =
        If(
            IfExpr(
                IfKeyword at,
                I32Eqz(Var(n, ref None, at), at),
                Block(OpenBrace pos, ([], I32Const(1, at)), CloseBrace pos),
                Block(
                    OpenBrace pos,
                    ([],
                     I32Mul(
                         Var(n, ref None, at),
                         I32MulOp pos,
                         Application(fact, [ I32Sub(Var(n, ref None, at), I32SubOp pos, I32Const(1, at)) ], at)
                     )),
                    CloseBrace pos
                )
            )
        )

    let (simulEquation, _ty) = extract funcContext varContext expr |> wantOk

    let assigns = unify simulEquation |> wantOk

    let dereferenced = expr |> derefType assigns |> wantOk

    SnapshotTest.VerifyJSON(encodeExpr dereferenced)

[<EntryPoint>]
let main _ = 0

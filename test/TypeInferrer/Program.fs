module Waxt.TypeInferrer.Test.Program

open NUnit.Framework
open Waxt.Ast
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

    let fact = Ident.make "fact" at
    let n = Ident.make "n" at

    let t0 = TyVar(TyVarName.make "t0")
    let t1 = TyVar(TyVarName.make "t1")

    let funcContext =
        FuncContext.empty
        |> FuncContext.add fact (FuncType([ t0 ], t1))

    let varContext = VarContext.empty |> VarContext.add n t0

    let expr =
        If(
            IfExpr.make
                (Ident.make "if" at)
                (I32Eqz(
                    Parenthesized.make
                        (LeftParen.make pos)
                        {| Ident = Ident.make "i32_eqz" at
                           Arg = Var(n, ref None) |}
                        (RightParen.make pos)
                ))
                (Block.make (LeftParen.make pos) [ I32Const(1, at) ] (RightParen.make pos))
                (Block.make
                    (LeftParen.make pos)
                    [ I32Mul(
                          Parenthesized.make
                              (LeftParen.make pos)
                              {| Ident = Ident.make "i32_mul" at
                                 Lhs = Var(n, ref None)
                                 Rhs =
                                  Application(
                                      Parenthesized.make
                                          (LeftParen.make pos)
                                          {| FuncName = fact
                                             Args =
                                              [ I32Sub(
                                                    Parenthesized.make
                                                        (LeftParen.make pos)
                                                        {| Ident = Ident.make "i32_sub" at
                                                           Lhs = Var(n, ref None)
                                                           Rhs = I32Const(1, at) |}
                                                        (RightParen.make pos)
                                                ) ] |}
                                          (RightParen.make pos)
                                  ) |}
                              (RightParen.make pos)
                      ) ]
                    (RightParen.make pos))
        )

    let (simulEquation, _ty) = extract funcContext varContext expr |> wantOk

    let assigns = unify simulEquation |> wantOk

    let dereferenced = expr |> derefType assigns |> wantOk

    SnapshotTest.VerifyJSON(FixedExpr.encodeExpr dereferenced)

[<EntryPoint>]
let main _ = 0

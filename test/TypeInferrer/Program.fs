module WAXT.TypeInferrer.Test.Program

open NUnit.Framework
open WAXT.Location
open WAXT.TestUtil
open WAXT.Token
open WAXT.Type
open WAXT.TypeInferrer
open WAXT.TypeInferrer.DerefType
open WAXT.TypeInferrer.Extract
open WAXT.TypeInferrer.Unify
open WAXT.AST

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

    let term =
        If(
            IfExpr(
                IfKeyword at,
                I32Eqz(Var(n, ref None, at), at),
                {| OpenBrace = OpenBrace pos
                   Body = I32Const(1, at)
                   CloseBrace = CloseBrace pos |},
                {| OpenBrace = OpenBrace pos
                   Body =
                    I32Mul(
                        Var(n, ref None, at),
                        I32MulOp pos,
                        Application(fact, [ I32Sub(Var(n, ref None, at), I32SubOp pos, I32Const(1, at)) ], at)
                    )
                   CloseBrace = CloseBrace pos |}
            )
        )

    let (simulEquation, _ty) = extract funcContext varContext term |> wantOk

    let assigns = unify simulEquation |> wantOk

    let dereferenced =
        term
        |> derefType assigns
        |> wantOk
        |> DereferencedTerm.toJSON

    SnapshotTest.VerifyJSON dereferenced

[<EntryPoint>]
let main _ = 0

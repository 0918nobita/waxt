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
    let at = Range.fromPos Pos.origin

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
            I32Eqz(Var(n, ref None, at), at),
            I32Const(1, at),
            I32Mul(
                Var(n, ref None, at),
                I32MulOp Pos.origin,
                Application(fact, [ I32Sub(Var(n, ref None, at), I32SubOp Pos.origin, I32Const(1, at)) ], at)
            ),
            at
        )

    let (simulEquation, _ty) = extract funcContext varContext term |> wantOk

    let assigns = unify simulEquation |> wantOk

    let dereferenced =
        term
        |> derefType assigns
        |> wantOk
        |> DereferencedTerm.toJson

    SnapshotTest.VerifyJSON dereferenced

[<EntryPoint>]
let main _ = 0

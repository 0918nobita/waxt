module WAXT.TypeInferrer.Test.Program

open NUnit.Framework
open WAXT.Location
open WAXT.Type
open WAXT.TypeInferrer
open WAXT.TypeInferrer.Extract
open WAXT.TypeInferrer.Unify
open WAXT.AST

[<SetUp>]
let Setup () = ()

let wantOk (res: Result<'a, 'b>) : 'a =
    match res with
    | Ok value -> value
    | Error _ ->
        Assert.Fail "Expected Ok"
        failwith "unreachable"

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

    let ast =
        If(
            I32Eqz(Var(n, ref None, at), at),
            I32Const(1, at),
            I32Mul(
                Var(n, ref None, at),
                Application(fact, [ I32Sub(Var(n, ref None, at), I32Const(1, at), at) ], at),
                at
            ),
            at
        )

    let (simulEquation, ty) = extract funcContext varContext ast |> wantOk

    printfn "%O" simulEquation
    printfn "%O" ty
    printfn "%O" ast

    unify simulEquation |> wantOk |> printfn "%A"

    Assert.Pass()

[<EntryPoint>]
let main _ = 0

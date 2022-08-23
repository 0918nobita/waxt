[<AutoOpen>]
module Waxt.CodeGen.Library

open Waxt.Wasm

module WaxtType = Waxt.Type.Type

module TypedAst = Waxt.TypedAst.Library

let private waxtTypeToWasmType (waxtType: WaxtType.Type) =
    match waxtType with
    | WaxtType.I32 -> I32
    | WaxtType.I64 -> I64
    | _ -> failwith "Not implemented"

let private funcSigToFuncType (TypedAst.FuncSig (parameters, result, _)) : FunctionType =
    let parameters =
        parameters
        |> Seq.map (fun (_, (_, (ty, _))) -> waxtTypeToWasmType ty)
        |> List.ofSeq

    let result = waxtTypeToWasmType result
    FunctionType(Vec parameters, result)

let rec private genInsts (typedExpr: TypedAst.TypedExpr) : list<Inst> =
    match typedExpr with
    | TypedAst.I32Const (n, _) -> [ I32Const n ]
    | TypedAst.I32Add (lhs, rhs, _) ->
        let lhs = genInsts lhs
        let rhs = genInsts rhs
        lhs @ rhs @ [ I32Add ]
    | TypedAst.I32Mul (lhs, rhs, _) ->
        let lhs = genInsts lhs
        let rhs = genInsts rhs
        lhs @ rhs @ [ I32Mul ]
    | TypedAst.Var (index, _, _) -> [ LocalGet(uint32 index) ]
    | _ -> failwith "Not implemented"

type FuncTyPool = list<FunctionType>

type CompiledFunc = TypeIndex * Code

type State = FuncTyPool * list<CompiledFunc>

let genCode (funcs: TypedAst.TypedFuncs) : Wasm =
    let (funcTyPool, compiledFuncs) =
        (([], []: State), funcs)
        ||> Seq.fold (fun (funcTyPool, compiledFuncs) (_, (funcSig, funcBody)) ->
            let funcType = funcSigToFuncType funcSig

            let (funcTyPool, index) =
                List.tryFindIndex ((=) funcType) funcTyPool
                |> Option.map (fun index -> (funcTyPool, index))
                |> Option.defaultWith (fun () -> (funcTyPool @ [ funcType ], List.length funcTyPool))

            let insts = funcBody |> List.map genInsts |> List.concat

            let compiledFunc = (TypeIndex(uint32 index), Code(Func(Vec [], Expr insts)))
            (funcTyPool, compiledFunc :: compiledFuncs))

    let compiledFuncs = List.rev compiledFuncs

    let typeSection = TypeSection(Vec funcTyPool)

    let functionSection = FunctionSection(Vec(compiledFuncs |> List.map fst))

    let codeSection = CodeSection(Vec(compiledFuncs |> List.map snd))

    Wasm(typeSection, functionSection, codeSection)

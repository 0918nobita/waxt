[<AutoOpen>]
module Waxt.CodeGen.Library

open FsToolkit.ErrorHandling
open Waxt.Location
open Waxt.Wasm

module WaxtType = Waxt.Type.Type

module TypedAst = Waxt.TypedAst.Library

type CodeGenError = CodeGenError of msg: string * at: Range

module CodeGenError =
    let toString (CodeGenError (msg, at)) =
        let at = Range.toString at
        $"(%s{at}) %s{msg}"

let private waxtTypeToWasmResultType (waxtType: WaxtType.Type) : Vector<NumberType> =
    match waxtType with
    | WaxtType.I32 -> [ I32 ]
    | WaxtType.I64 -> [ I64 ]
    | WaxtType.Unit -> []
    |> Vec

let private funcSigToFuncType (TypedAst.FuncSig (parameters, resultTy, _)) : Result<FunctionType, list<CodeGenError>> =
    result {
        let! parameters =
            parameters
            |> List.ofSeq
            |> List.map (fun (_, (_, (ty, at))) ->
                match ty with
                | WaxtType.I32 -> Ok I32
                | WaxtType.I64 -> Ok I64
                | WaxtType.Unit -> Error(CodeGenError("unit cannot be used as a parameter type", at)))
            |> List.sequenceResultA

        let result = waxtTypeToWasmResultType resultTy
        return FunctionType(Vec parameters, result)
    }

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

    | TypedAst.I32Store (addr, content, _) ->
        let addr = genInsts addr
        let content = genInsts content
        addr @ content @ [ I32Store ]

type FuncTyPool = list<FunctionType>

type CompiledFunc = TypeIndex * Code

type State = FuncTyPool * list<CompiledFunc>

let genCode (funcs: TypedAst.TypedFuncs) : Result<Wasm, list<CodeGenError>> =
    result {
        let! (funcTyPool, compiledFuncs) =
            (Ok([], []: State), funcs)
            ||> Seq.fold (fun state (_, (funcSig, funcBody)) ->
                result {
                    let! (funcTyPool, compiledFuncs) = state
                    let! funcType = funcSigToFuncType funcSig

                    let (funcTyPool, index) =
                        List.tryFindIndex ((=) funcType) funcTyPool
                        |> Option.map (fun index -> (funcTyPool, index))
                        |> Option.defaultWith (fun () -> (funcTyPool @ [ funcType ], List.length funcTyPool))

                    let insts = funcBody |> List.map genInsts |> List.concat

                    let compiledFunc = (TypeIndex(uint32 index), Code(Func(Vec [], Expr insts)))
                    return (funcTyPool, compiledFunc :: compiledFuncs)
                })

        let compiledFuncs = List.rev compiledFuncs

        let typeSection = TypeSection(Vec funcTyPool)

        let functionSection = FunctionSection(Vec(compiledFuncs |> List.map fst))

        let codeSection = CodeSection(Vec(compiledFuncs |> List.map snd))

        return Wasm(typeSection, functionSection, codeSection)
    }

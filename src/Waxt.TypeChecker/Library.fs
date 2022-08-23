[<AutoOpen>]
module Waxt.TypeChecker.Library

open FsToolkit.ErrorHandling
open Waxt.TypedAst
open Waxt.UntypedAst

let typeFuncDefs (funcDefs: seq<FuncDef>) : Result<TypedFuncs, TypeError> =
    result {
        let! untypedFuncs = typeFuncSigs funcDefs
        return! typeFuncBodies untypedFuncs
    }

[<AutoOpen>]
module Waxt.TypeChecker.CheckAst

open Waxt.Type
open Waxt.TypedAst
open Waxt.UntypedAst

type FuncSig = private FuncSig of name: string * parameters: list<string * Type> * result: Type

type FuncIndex = private FuncIndex of int

module FuncIndex =
    let make index = FuncIndex index

type FuncName = private FuncName of name: string

module FuncName =
    let make name = FuncName name

type FuncNameIndexMap = private FuncNameIndexMap of Map<FuncName, FuncIndex>

type FuncSigCollection = private FuncSigCollection of list<FuncSig>

module FuncSigCollection =
    let tryGet (FuncIndex index) (FuncSigCollection sigs) = List.tryItem index sigs

let checkAst (stmts: list<Stmt>) : list<TypedExpr> = failwith "not implemented"

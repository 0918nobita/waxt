[<AutoOpen>]
module Waxt.UntypedAst.FuncDef

open Waxt.Type

type FuncDef = FuncDef of name: FuncName * resultTy: Type * parameters: list<string * Type> * body: list<Expr>

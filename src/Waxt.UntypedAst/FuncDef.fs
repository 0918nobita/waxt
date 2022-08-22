[<AutoOpen>]
module Waxt.UntypedAst.FuncDef

open Waxt.Location
open Waxt.Type

type FuncDef =
    | FuncDef of
        name: FuncName *
        resultTy: Type *
        parameters: list<(string * Range) * (Type * Range)> *
        body: list<Expr>

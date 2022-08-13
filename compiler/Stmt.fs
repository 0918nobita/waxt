module Waxt.Compiler.Stmt

open Location

type Ty = I32

type Expr =
    | I32Add of lhs: Expr * rhs: Expr * at: Range
    | I32Const of value: int * at: Range
    | I32Mul of lhs: Expr * rhs: Expr * at: Range
    | I32Store of addr: Expr * content: Expr * at: Range
    | Var of name: string * at: Range

type Stmt = FuncDef of name: string * result: option<Ty> * parameters: list<string * Ty> * body: list<Expr>

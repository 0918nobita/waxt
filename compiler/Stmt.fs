module Waxt.Compiler.Stmt

type Ty = I32

type Expr =
    | I32Const of int
    | I32Add of lhs: Expr * rhs: Expr

type Stmt = FuncDef of name: string * result: option<Ty> * parameters: list<string * Ty> * body: list<Expr>

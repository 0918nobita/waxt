module Waxt.TypeInferrer.Term

open FuncContext
open TypeLiteral
open VarContext

type Term =
    | I32Const of n: int
    | I32Eqz of Term
    | I32Add of lhs: Term * rhs: Term
    | I32Sub of lhs: Term * rhs: Term
    | I32Mul of lhs: Term * rhs: Term
    | If of cond: Term * thenClause: Term * elseClause: Term
    | Let of varName: VarName * value: Term * body: Term
    | LetWithType of varName: VarName * tyLit: TypeLiteral * value: Term * body: Term
    | Application of funcName: FuncName * args: list<Term>
    | Var of name: VarName

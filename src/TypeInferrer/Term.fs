[<AutoOpen>]
module Waxt.TypeInferrer.Term

type Term =
    | I32Const of n: int
    | I32Eqz of Term
    | I32Add of lhs: Term * rhs: Term
    | I32Sub of lhs: Term * rhs: Term
    | I32Mul of lhs: Term * rhs: Term
    | If of cond: Term * thenClause: Term * elseClause: Term
    | Let of name: string * value: Term * body: Term
    | LetWithType of name: string * tyLit: TypeLiteral * value: Term * body: Term
    | Application of funcName: string * args: list<Term>
    | Var of name: string

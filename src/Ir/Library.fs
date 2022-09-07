namespace Waxt.Ir

open Waxt.Type

type LocalIndex = LocalIndex of uint

type FuncIndex = FuncIndex of uint

type Term =
    | I32Const of n: int
    | I32Eqz of Term
    | I32Add of lhs: Term * rhs: Term
    | I32Sub of lhs: Term * rhs: Term
    | I32Mul of lhs: Term * rhs: Term
    | If of resultType: NumType * cond: Term * thenClause: list<Term> * elseClause: list<Term>
    | Application of FuncIndex * args: list<Term>
    | LocalGet of NumType * LocalIndex
    | LocalSet of NumType * LocalIndex * Term

type FuncParams = list<NumType>

type FuncDef = FuncDef of FuncParams * ret: option<NumType> * body: list<Term>

type Module = list<FuncDef>

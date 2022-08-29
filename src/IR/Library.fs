namespace WAXT.IR

open WAXT.Type

type LocalIndex = LocalIndex of uint

type FuncIndex = FuncIndex of uint

type TypedTerm =
    | I32Const of n: int
    | I32Eqz of TypedTerm
    | I32Add of lhs: TypedTerm * rhs: TypedTerm
    | I32Sub of lhs: TypedTerm * rhs: TypedTerm
    | I32Mul of lhs: TypedTerm * rhs: TypedTerm
    | If of cond: TypedTerm * thenClause: TypedTerm * elseClause: TypedTerm
    | Let of LocalIndex * boundValue: TypedTerm * body: TypedTerm
    | Application of FuncIndex * args: list<TypedTerm>
    | Var of LocalIndex

type FuncParams = list<Type>

type FuncDef = FuncDef of FuncParams * ret: option<Type> * body: list<TypedTerm>

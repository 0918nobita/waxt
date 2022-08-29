namespace WAXT.UntypedAst

open WAXT.Location
open WAXT.NumType

type Term =
    | I32Const of n: int * at: Range
    | I32Eqz of Term * at: Range
    | I32Add of lhs: Term * rhs: Term * at: Range
    | I32Sub of lhs: Term * rhs: Term * at: Range
    | I32Mul of lhs: Term * rhs: Term * at: Range
    | If of cond: Term * thenClause: Term * elseClause: Term * at: Range
    | Let of VarName * boundValue: Term * body: Term * at: Range
    | LetWithType of VarName * TypeLiteral * value: Term * body: Term * at: Range
    | Application of FuncName * args: list<Term> * at: Range
    | Var of VarName * ty: ref<option<NumType>> * at: Range

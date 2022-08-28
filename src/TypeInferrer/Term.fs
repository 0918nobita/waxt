namespace Waxt.TypeInferrer

type Term =
    | I32Const of n: int
    | I32Eqz of Term
    | I32Add of lhs: Term * rhs: Term
    | I32Sub of lhs: Term * rhs: Term
    | I32Mul of lhs: Term * rhs: Term
    | If of cond: Term * thenClause: Term * elseClause: Term
    | Let of VarName * boundValue: Term * body: Term
    | LetWithType of VarName * TypeLiteral * value: Term * body: Term
    | Application of FuncName * args: list<Term>
    | Var of VarName

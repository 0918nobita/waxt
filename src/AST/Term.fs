namespace WAXT.AST

open WAXT.Location
open WAXT.Type

type Term<'Ty> =
    | I32Const of n: int * at: Range
    | I32Eqz of Term<'Ty> * at: Range
    | I32Add of lhs: Term<'Ty> * rhs: Term<'Ty> * at: Range
    | I32Sub of lhs: Term<'Ty> * rhs: Term<'Ty> * at: Range
    | I32Mul of lhs: Term<'Ty> * rhs: Term<'Ty> * at: Range
    | If of cond: Term<'Ty> * thenClause: Term<'Ty> * elseClause: Term<'Ty> * at: Range
    | Let of VarName * boundValue: Term<'Ty> * body: Term<'Ty> * at: Range
    | LetWithType of VarName * TypeLiteral * value: Term<'Ty> * body: Term<'Ty> * at: Range
    | Application of FuncName * args: list<Term<'Ty>> * at: Range
    | Var of VarName * ty: 'Ty * at: Range

type MutableTerm = Term<ref<option<Type>>>

type DereferencedTerm = Term<Type>

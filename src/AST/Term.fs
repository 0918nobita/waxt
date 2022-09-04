namespace WAXT.AST

open WAXT.Location
open WAXT.Token
open WAXT.Type

type Term<'Ty> =
    | I32Const of n: int * at: Range

    | I32Eqz of Term<'Ty> * at: Range

    | I32Add of lhs: Term<'Ty> * op: I32AddOp * rhs: Term<'Ty>
    | I32Sub of lhs: Term<'Ty> * op: I32SubOp * rhs: Term<'Ty>
    | I32Mul of lhs: Term<'Ty> * op: I32MulOp * rhs: Term<'Ty>

    | If of IfExpr<'Ty>

    | Let of VarName * boundValue: Term<'Ty> * body: Term<'Ty> * at: Range
    | LetWithType of VarName * TypeLiteral * value: Term<'Ty> * body: Term<'Ty> * at: Range

    | Application of FuncName * args: list<Term<'Ty>> * at: Range

    | Var of VarName * ty: 'Ty * at: Range

and IfExpr<'Ty> =
    | IfExpr of
        if_: IfKeyword *
        cond: Term<'Ty> *
        thenClause: {| OpenBrace: OpenBrace
                       Body: Term<'Ty>
                       CloseBrace: CloseBrace |} *
        elseClause: {| OpenBrace: OpenBrace
                       Body: Term<'Ty>
                       CloseBrace: CloseBrace |}

type MutableTerm = Term<ref<option<Type>>>

type DereferencedTerm = Term<Type>

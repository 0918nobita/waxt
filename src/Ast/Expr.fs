namespace Waxt.Ast

open Waxt.Location
open Waxt.Token
open Waxt.Type

type Expr<'Ty> =
    | I32Const of n: int * at: Range

    | I32Eqz of Expr<'Ty> * at: Range

    | I32Add of lhs: Expr<'Ty> * op: I32AddOp * rhs: Expr<'Ty>
    | I32Sub of lhs: Expr<'Ty> * op: I32SubOp * rhs: Expr<'Ty>
    | I32Mul of lhs: Expr<'Ty> * op: I32MulOp * rhs: Expr<'Ty>

    | If of IfExpr<'Ty>

    | Let of VarName * boundValue: Expr<'Ty> * body: Expr<'Ty> * at: Range
    | LetWithType of VarName * TypeLiteral * value: Expr<'Ty> * body: Expr<'Ty> * at: Range

    | Application of FuncName * args: list<Expr<'Ty>> * at: Range

    | Var of VarName * ty: 'Ty * at: Range

and IfExpr<'Ty> = IfExpr of if_: IfKeyword * cond: Expr<'Ty> * thenClause: Block<'Ty> * elseClause: Block<'Ty>

and Block<'Ty> = Block of openBrace: OpenBrace * body: (list<Expr<'Ty>> * Expr<'Ty>) * closeBrace: CloseBrace

type MutableBlock = Block<ref<option<Type>>>

type MutableExpr = Expr<ref<option<Type>>>

type FixedIfExpr = IfExpr<Type>

type FixedBlock = Block<Type>

type FixedExpr = Expr<Type>

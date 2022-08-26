module Waxt.TypeInferrer

type TyVarName = TyVarName of string

type Type =
    | I32
    | I64
    | F32
    | F64
    | TyVar of name: string
    | Func of args: list<Type> * ret: Type

type VarName = VarName of string
type FuncName = FuncName of string

type Term =
    | I32Const of n: int
    | Var of name: VarName
    | I32Eqz of Term
    | I32Add of lhs: Term * rhs: Term
    | I32Sub of lhs: Term * rhs: Term
    | I32Mul of lhs: Term * rhs: Term
    | If of cond: Term * thenClause: Term * elseClause: Term
    | Let of name: VarName * value: Term * body: Term
    | LetWithType of name: VarName * ty: Type * value: Term * body: Term
    | Application of funcName: FuncName * args: list<Term>

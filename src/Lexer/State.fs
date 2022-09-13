namespace Waxt.Lexer

open Waxt.Location

type LexState =
    | Initial
    | ReadingMinus of startPos: Pos
    | ReadingI32Lit of startPos: Pos * abs: int * neg: bool
    | ReadingIdent of startPos: Pos * raw: string

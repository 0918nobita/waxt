namespace Waxt.Lexer

open Waxt.Location

type LexError = LexError of msg: string * at: Pos

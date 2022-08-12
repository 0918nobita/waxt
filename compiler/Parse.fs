module Waxt.Compiler.Parse

open Location

type ParseError = ParseError of msg: string * at: Range

type ParseResult<'T> = Result<'T, ParseError>

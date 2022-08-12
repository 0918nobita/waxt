module Waxt.Compiler.Parse

open Location

type IParseError =
    abstract member Msg: string
    inherit ILocatable

type ParseResult<'T> = Result<'T, IParseError>

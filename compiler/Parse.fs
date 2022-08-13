[<AutoOpen>]
module Waxt.Compiler.Parse

type ParseError = ParseError of msg: string * at: Range

module ParseError =
    let toString (ParseError (msg, at)) =
        let at = Range.toString at
        $"(%s{at}) %s{msg}"

type ParseResult<'T> = Result<'T, ParseError>

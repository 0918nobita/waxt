[<AutoOpen>]
module Waxt.Parser.Parse

open Waxt.Location

type ParseError = ParseError of msg: string * at: Range

module ParseError =
    let toString (ParseError (msg, at)) =
        let at = Range.toString at
        $"(%s{at}) %s{msg}"

type ParseResult<'T> = Result<'T, ParseError>

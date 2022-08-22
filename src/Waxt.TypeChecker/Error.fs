[<AutoOpen>]
module Waxt.TypeChecker.Error

open Waxt.Location

type TypeError = TypeError of msg: string * at: Range

module TypeError =
    let toString (TypeError (msg, at)) =
        let at = Range.toString at
        $"(%s{at}) %s{msg}"

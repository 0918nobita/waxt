[<AutoOpen>]
module Waxt.TypeChecker.Error

open Waxt.Location

type TypeError = TypeError of msg: string * at: option<Range>

[<AutoOpen>]
module Waxt.UntypedAst.FuncName

open Thoth.Json.Net
open Waxt.Location

type FuncName = FuncName of name: string * at: Range

module FuncName =
    let toJson =
        function
        | FuncName (name, at) ->
            Encode.object [ "type", Encode.string "funcName"
                            "name", Encode.string name
                            "at", Encode.string (Range.toString at) ]

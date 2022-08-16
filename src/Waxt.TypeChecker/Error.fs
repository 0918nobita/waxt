[<AutoOpen>]
module Waxt.TypeChecker.Error

open Waxt.Location

type TypeError = TypeError of msg: string * at: option<Range>

module TypeError =
    let toString (TypeError (msg, at)) =
        let at =
            at
            |> Option.map (fun range -> range |> Range.toString |> sprintf "(%s) ")
            |> Option.defaultValue ""

        $"%s{at} %s{msg}"

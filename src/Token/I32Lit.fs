namespace Waxt.Token

open Thoth.Json.Net
open Waxt.Location

type I32Lit = I32Lit of n: int * at: Range

module I32Lit =
    let toJSON (I32Lit (n, at)) =
        [ "type", Encode.string "i32Lit"
          "value", Encode.int n
          "at", Range.toJSON at ]
        |> Encode.object

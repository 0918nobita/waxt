namespace Waxt.Token

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif
open Waxt.Location

type I32Lit = private I32Lit of n: int * at: Range

module I32Lit =
    let make n at = I32Lit(n, at)

    let toJSON (I32Lit (n, at)) =
        [ "type", Encode.string "i32Lit"
          "value", Encode.int n
          "at", Range.toJSON at ]
        |> Encode.object

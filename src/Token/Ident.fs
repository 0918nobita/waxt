namespace Waxt.Token

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif
open Waxt.Location

type Ident = private Ident of raw: string * at: Range

module Ident =
    let make raw at = Ident(raw, at)

    let locate (Ident (_, at)) = at

    let toJSON (Ident (raw, at)) =
        [ "type", Encode.string "ident"
          "raw", Encode.string raw
          "at", Range.toJSON at ]
        |> Encode.object

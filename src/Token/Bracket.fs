namespace Waxt.Token

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif
open Waxt.Location

type LeftBracket = private LeftBracket of Pos

module LeftBracket =
    let make pos = LeftBracket pos

    let locate (LeftBracket pos) = Range.fromPos pos

    let toJSON (LeftBracket pos) =
        [ "type", Encode.string "leftBracket"
          "at", Pos.toJSON pos ]
        |> Encode.object

type RightBracket = RightBracket of Pos

module RightBracket =
    let make pos = RightBracket pos

    let locate (RightBracket pos) = Range.fromPos pos

    let toJSON (RightBracket pos) =
        [ "type", Encode.string "rightBracket"
          "at", Pos.toJSON pos ]
        |> Encode.object

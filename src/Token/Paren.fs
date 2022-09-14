namespace Waxt.Token

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif
open Waxt.Location

type LeftParen = private LeftParen of Pos

module LeftParen =
    let make pos = LeftParen pos

    let locate (LeftParen pos) = Range.fromPos pos

    let toJSON (LeftParen pos) =
        [ "type", Encode.string "leftParen"
          "at", Pos.toJSON pos ]
        |> Encode.object

type RightParen = private RightParen of Pos

module RightParen =
    let make pos = RightParen pos

    let locate (RightParen pos) = Range.fromPos pos

    let toJSON (RightParen pos) =
        [ "type", Encode.string "rightParen"
          "at", Pos.toJSON pos ]
        |> Encode.object

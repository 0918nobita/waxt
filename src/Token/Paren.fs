namespace Waxt.Token

open Thoth.Json.Net
open Waxt.Location

type LeftParen = LeftParen of Pos

module LeftParen =
    let locate (LeftParen pos) = Range.fromPos pos

    let toJSON (LeftParen pos) =
        [ "type", Encode.string "leftParen"
          "at", Pos.toJSON pos ]
        |> Encode.object

type RightParen = RightParen of Pos

module RightParen =
    let locate (RightParen pos) = Range.fromPos pos

    let toJSON (RightParen pos) =
        [ "type", Encode.string "rightParen"
          "at", Pos.toJSON pos ]
        |> Encode.object

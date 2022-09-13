namespace Waxt.Token

open Thoth.Json.Net
open Waxt.Location

type LeftBracket = LeftBracket of Pos

module LeftBracket =
    let locate (LeftBracket pos) = Range.fromPos pos

    let toJSON (LeftBracket pos) =
        [ "type", Encode.string "leftBracket"
          "at", Pos.toJSON pos ]
        |> Encode.object

type RightBracket = RightBracket of Pos

module RightBracket =
    let locate (RightBracket pos) = Range.fromPos pos

    let toJSON (RightBracket pos) =
        [ "type", Encode.string "rightBracket"
          "at", Pos.toJSON pos ]
        |> Encode.object

namespace WAXT.Token

open WAXT.Location

type OpenBrace = OpenBrace of Pos

module OpenBrace =
    let locate (OpenBrace pos) = Range.fromPos pos

type CloseBrace = CloseBrace of Pos

module CloseBrace =
    let locate (CloseBrace pos) = Range.fromPos pos

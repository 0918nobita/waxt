namespace Waxt.Location

open Thoth.Json.Net

type Pos =
    private
    | Pos of line: int * column: int

    override this.ToString() =
        match this with
        | Pos (line, column) ->
            let line = line + 1
            let column = column + 1
            $"%i{line}:%i{column}"

module Pos =
    let make line column = Pos(line, column)

    let origin = Pos(0, 0)

    let
#if !DEBUG
    inline
#endif
        toJSON (pos: Pos) = pos |> string |> Encode.string

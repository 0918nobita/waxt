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
        nextLine (Pos (line, _)) = Pos(line + 1, 0)

    let
#if !DEBUG
    inline
#endif
        nextColumn (Pos (line, column)) = Pos(line, column + 1)

    let
#if !DEBUG
    inline
#endif
        previousColumn (Pos (line, column)) = Pos(line, column - 1)

    let
#if !DEBUG
    inline
#endif
        toJSON (pos: Pos) = pos |> string |> Encode.string

namespace Waxt.Location

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

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

    let nextLine (Pos (line, _)) = Pos(line + 1, 0)

    let nextColumn (Pos (line, column)) = Pos(line, column + 1)

    let previousColumn (Pos (line, column)) = Pos(line, column - 1)

    let toJSON (pos: Pos) = pos |> string |> Encode.string

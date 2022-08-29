namespace Waxt.Location

type Pos =
    private
    | Pos of line: int * column: int

    override this.ToString() =
        match this with
        | Pos (line, column) -> $"%i{line}:%i{column}"

module Pos =
    let make line column = Pos(line, column)

    let origin = Pos(0, 0)

[<AutoOpen>]
module Waxt.Compiler.Location

type Pos = Pos of line: uint * column: uint

module Pos =
    let toString (Pos (line, column)) =
        let line = line + 1u
        let column = column + 1u
        $"%i{line}:%i{column}"

    let nextColumn (Pos (line, column)) = Pos(line, column + 1u)

    let previousColumn (Pos (line, column)) = Pos(line, column - 1u)

    let nextLine (Pos (line, _)) = Pos(line + 1u, 0u)

type Range = Range of start: Pos * ``end``: Pos

module Range =
    let toString (Range (start, ``end``)) =
        let start = Pos.toString start
        let ``end`` = Pos.toString ``end``
        $"{start}-{``end``}"

    let fromPos p = Range(p, p)

type ILocatable =
    abstract member Locate: unit -> Range

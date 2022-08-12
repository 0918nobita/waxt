module Waxt.Compiler.Location

type Point = Point of line: uint * column: uint

module Point =
    let toString (Point (line, column)) =
        let line = line + 1u
        let column = column + 1u
        $"%i{line}:%i{column}"

    let nextColumn (Point (line, column)) = Point(line, column + 1u)

    let previousColumn (Point (line, column)) = Point(line, column - 1u)

    let nextLine (Point (line, _)) = Point(line + 1u, 0u)

type Range = Range of start: Point * ``end``: Point

module Range =
    let toString (Range (start, ``end``)) =
        let start = Point.toString start
        let ``end`` = Point.toString ``end``
        $"{start}-{``end``}"

    let fromPoint p = Range(p, p)

type ILocatable =
    abstract member Locate: unit -> Range

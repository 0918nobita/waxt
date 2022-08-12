module Waxt.Compiler.Location

type Point = Point of line: uint * column: uint

module Point =
    let nextColumn (Point (line, column)) = Point(line, column + 1u)

    let previousColumn (Point (line, column)) = Point(line, column - 1u)

    let nextLine (Point (line, _)) = Point(line + 1u, 0u)

type Range = Range of start: Point * ``end``: Point

module Range =
    let fromPoint p = Range(p, p)

type ILocatable =
    abstract member Locate: unit -> Range

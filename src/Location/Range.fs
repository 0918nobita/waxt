namespace Waxt.Location

open Thoth.Json.Net

type Range =
    private
    | Range of start: Pos * end_: Pos

    override this.ToString() =
        match this with
        | Range (start, end_) -> $"%O{start}-%O{end_}"

module Range =
    let make start end_ = Range(start, end_)

    let fromPos pos = Range(pos, pos)

    let
#if !DEBUG
    inline
#endif
        toJSON (range: Range) = range |> string |> Encode.string

    let combine (range1: Range) (range2: Range) =
        match range1, range2 with
        | Range (start, _), Range (_, end_) -> Range(start, end_)

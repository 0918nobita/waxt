namespace Waxt.Location

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

type Range =
    private
    | Range of start: Pos * end_: Pos

    override this.ToString() =
        match this with
        | Range (start, end_) -> $"%O{start}-%O{end_}"

module Range =
    let make start end_ = Range(start, end_)

    let fromPos pos = Range(pos, pos)

    let toJSON (range: Range) = range |> string |> Encode.string

    let combine (range1: Range) (range2: Range) =
        match range1, range2 with
        | Range (start, _), Range (_, end_) -> Range(start, end_)

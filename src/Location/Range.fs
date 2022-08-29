namespace Waxt.Location

type Range =
    private
    | Range of start: Pos * end_: Pos

    override this.ToString() =
        match this with
        | Range (start, end_) -> $"%O{start}-%O{end_}"

module Range =
    let make start end_ = Range(start, end_)

    let fromPos pos = Range(pos, pos)

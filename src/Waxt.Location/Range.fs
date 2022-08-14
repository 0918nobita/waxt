[<AutoOpen>]
module Waxt.Location.Range

/// ソースファイル上の範囲
type Range = private Range of start: Pos * ``end``: Pos

module Range =
    let make start ``end`` = Range(start, ``end``)

    let start (Range (start, _)) = start

    let ``end`` (Range (_, ``end``)) = ``end``

    let toString (Range (start, ``end``)) =
        let start = Pos.toString start
        let ``end`` = Pos.toString ``end``
        $"{start}-{``end``}"

    let fromPos p = Range(p, p)

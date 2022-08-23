[<AutoOpen>]
module Waxt.Wasm.Section

open Leb128

type ISection =
    abstract member Id: byte
    abstract member GetContents: unit -> list<byte>

module Section =
    let toBytes (section: ISection) : list<byte> =
        let contents = section.GetContents()

        let len =
            contents
            |> List.length
            |> uint32
            |> unsignedLeb128

        section.Id :: len @ contents

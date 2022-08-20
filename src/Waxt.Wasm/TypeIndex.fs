[<AutoOpen>]
module Waxt.Wasm.TypeIndex

open Leb128

type TypeIndex =
    private
    | TypeIndex of uint32

    interface ISerializable with
        member this.Serialize() =
            match this with
            | TypeIndex index -> unsignedLeb128 index

module TypeIndex =
    let make index = TypeIndex index

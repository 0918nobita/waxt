[<AutoOpen>]
module Waxt.Wasm.TypeIndex

open Leb128

type TypeIndex = private TypeIndex of uint32

module TypeIndex =
    let make index = TypeIndex index

    let toBytes (TypeIndex index) = unsignedLeb128 index

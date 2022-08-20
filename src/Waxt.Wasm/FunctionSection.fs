[<AutoOpen>]
module Waxt.Wasm.FunctionSection

open Leb128

type FunctionSection =
    private
    | FunctionSection of typeIndices: list<TypeIndex>

    interface ISection with
        member _.Id = 0x03uy

        member this.GetContents() =
            match this with
            | FunctionSection typeIndices ->
                let len =
                    typeIndices
                    |> List.length
                    |> uint32
                    |> unsignedLeb128

                let elements = typeIndices |> List.collect TypeIndex.toBytes
                len @ elements

module FunctionSection =
    let make typeIndices = FunctionSection typeIndices

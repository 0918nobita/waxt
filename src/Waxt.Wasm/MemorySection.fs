[<AutoOpen>]
module Waxt.Wasm.MemorySection

open Leb128

type MemType =
    | MemType of min: uint32

    interface ISerializable with
        member this.Serialize() =
            match this with
            | MemType min -> 0uy :: unsignedLeb128 min

type MemorySection =
    | MemorySection of memories: Vector<MemType>

    interface ISection with
        member _.Id = 0x05uy

        member this.GetContents() =
            match this with
            | MemorySection memories -> (memories :> ISerializable).Serialize()

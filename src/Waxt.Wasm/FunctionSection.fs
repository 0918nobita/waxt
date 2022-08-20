[<AutoOpen>]
module Waxt.Wasm.FunctionSection

type FunctionSection =
    | FunctionSection of typeIndices: Vector<TypeIndex>

    interface ISection with
        member _.Id = 0x03uy

        member this.GetContents() =
            match this with
            | FunctionSection typeIndices -> (typeIndices :> ISerializable).Serialize()

[<AutoOpen>]
module Waxt.Wasm.TypeSection

type TypeSection =
    | TypeSection of funcTypes: Vector<FunctionType>

    interface ISection with
        member _.Id = 1uy

        member this.GetContents() =
            match this with
            | TypeSection funcsTypes -> (funcsTypes :> ISerializable).Serialize()

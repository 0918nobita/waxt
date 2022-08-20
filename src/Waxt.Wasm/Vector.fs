[<AutoOpen>]
module Waxt.Wasm.Vector

open Leb128

type Vector<'T when 'T :> ISerializable> =
    private
    | Vec of list<'T>

    interface ISerializable with
        member this.Serialize() =
            match this with
            | Vec list ->
                let len = list |> List.length |> uint32 |> unsignedLeb128

                let elements =
                    list
                    |> List.collect (fun elem -> elem.Serialize())

                len @ elements

module Vector =
    let ofList list = Vec list

[<AutoOpen>]
module Waxt.Wasm.Serializable

type ISerializable =
    abstract member Serialize: unit -> list<byte>

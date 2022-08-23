[<AutoOpen>]
module Waxt.Wasm.Type

type NumberType =
    | I32
    | I64
    | F32
    | F64

    interface ISerializable with
        member this.Serialize() =
            match this with
            | I32 -> [ 0x7Fuy ]
            | I64 -> [ 0x7Euy ]
            | F32 -> [ 0x7Duy ]
            | F64 -> [ 0x7Cuy ]

type FunctionType =
    | FunctionType of parameter: Vector<NumberType> * result: Vector<NumberType>

    interface ISerializable with
        member this.Serialize() =
            match this with
            | FunctionType (parameter, result) ->
                0x60uy :: (parameter :> ISerializable).Serialize()
                @ (result :> ISerializable).Serialize()

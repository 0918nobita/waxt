module Waxt.Wasm.Leb128

open System

let private clearMsb (x: uint32) = x &&& 127u

let private addMsbFlags (bytes: list<uint8>) =
    let lastIndex = List.length bytes - 1

    bytes
    |> List.mapi (fun index part ->
        if index = lastIndex then
            part
        else
            part ||| 128uy)

let unsignedLeb128 (x: uint32) : list<byte> =
    let bytes =
        (clearMsb x)
        :: ([ x >>> 7
              x >>> 14
              x >>> 21
              x >>> 28 ]
            |> List.map clearMsb
            |> List.takeWhile ((<>) 0u))
        |> List.map uint8

    addMsbFlags bytes

let private reinterpretIntAsUint32 (x: int) : uint32 =
    x
    |> BitConverter.GetBytes
    |> Buffers.Binary.BinaryPrimitives.ReadUInt32LittleEndian

let signedLeb128 (x: int) : list<byte> =
    if x >= 0 then
        unsignedLeb128 (uint32 x)
    else
        let x = reinterpretIntAsUint32 x

        let bytes =
            (clearMsb x)
            :: ([ x >>> 7
                  x >>> 14
                  x >>> 21
                  x >>> 28 &&& 112u ]
                |> List.map clearMsb
                |> List.takeWhile ((<>) 127u))
            |> List.map uint8

        addMsbFlags bytes

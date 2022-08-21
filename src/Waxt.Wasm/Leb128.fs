module Waxt.Wasm.Leb128

let private splitPer7Bits (x: uint32) : list<byte> =
    if x = 0u then
        [ 0uy ]
    else
        let rec inner (shift: int) =
            match x >>> (7 * shift) with
            | 0u -> []
            | shifted ->
                uint8 (shifted &&& ~~~ 128u)
                :: (inner (shift + 1))

        inner 0

let private addMsbFlags (bytes: list<byte>) : list<byte> =
    let lastIndex = List.length bytes - 1

    bytes
    |> List.mapi (fun i byte ->
        if i = lastIndex then
            byte
        else
            byte ||| 128uy)

let unsignedLeb128 (x: uint32) : list<byte> = x |> splitPer7Bits |> addMsbFlags

open System

let private reinterpretIntAsUint32 (x: int) : uint32 =
    x
    |> BitConverter.GetBytes
    |> Buffers.Binary.BinaryPrimitives.ReadUInt32LittleEndian

let singedLeb128 (x: int) =
    if x >= 0 then
        unsignedLeb128 (uint32 x)
    else
        let x = reinterpretIntAsUint32 x

        // FIXME: フラグをつけるまえに 1 のみの 7 ビット列を捨てて、結果のバイト列をできる限り短くする
        [ x &&& 127u ||| 128u
          (x >>> 7) &&& 127u ||| 128u
          (x >>> 14) &&& 127u ||| 128u
          (x >>> 21) &&& 127u ||| 128u
          (x >>> 28) &&& 127u ||| 112u ]
        |> List.map uint8

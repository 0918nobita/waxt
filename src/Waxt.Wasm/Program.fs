module Waxt.Wasm

open System.IO

let splitPer7Bits (x: uint32) : list<byte> =
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

let addMsbFlags (bytes: array<byte>) : array<byte> =
    let lastIndex = Array.length bytes - 1

    bytes
    |> Array.mapi (fun i byte ->
        if i = lastIndex then
            byte
        else
            byte ||| 128uy)

let unsignedLeb128 (x: uint32) : array<byte> =
    x |> splitPer7Bits |> Array.ofList |> addMsbFlags

let writeHeader (writer: BinaryWriter) =
    writer.Write [| 0x00uy
                    0x61uy
                    0x73uy
                    0x6Duy |] // magic

    writer.Write 1u // version

let writeFunctionSection (writer: BinaryWriter) =
    writer.Write 0x03uy // id

    writer.Write [| 0x01uy |] // num bytes

    writer.Write [| 0x00uy |] // num function signatures

let () =
    let data = 471u
    printfn "       data: %i" data
    let bytes = splitPer7Bits data
    printfn "7 bit parts: %A" bytes
    let leb128 = unsignedLeb128 data
    printfn "     leb128: %A" leb128

    use file = File.Open("out.wasm", FileMode.Create)
    use writer = new BinaryWriter(file)
    writeHeader writer
    writeFunctionSection writer

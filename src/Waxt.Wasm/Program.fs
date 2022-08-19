module Waxt.Wasm

open System.IO

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
    use file = File.Open("out.wasm", FileMode.Create)
    use writer = new BinaryWriter(file)
    writeHeader writer
    writeFunctionSection writer

module Waxt.Wasm

open System.IO

let () =
    use file = File.Open("out.wasm", FileMode.OpenOrCreate)
    use writer = new BinaryWriter(file)
    writer.Write 0x00uy
    writer.Write 0x61uy
    writer.Write 0x73uy
    writer.Write 0x6Duy
    writer.Write 0x01uy
    writer.Write 0x00uy
    writer.Write 0x00uy
    writer.Write 0x00uy

module Waxt.Wasm.Program

open System.IO

let writeHeader (writer: BinaryWriter) =
    writer.Write [| 0x00uy
                    0x61uy
                    0x73uy
                    0x6Duy |] // magic

    writer.Write 1u // version

let () =
    use file = File.Open("out.wasm", FileMode.Create)
    use writer = new BinaryWriter(file)
    writeHeader writer

    let typeSection = TypeSection(Vec [ FunctionType(Vec [ I32 ], I32) ])
    writer.Write(Section.toBytes typeSection)

    let functionSection = FunctionSection(Vec [])
    writer.Write(Section.toBytes functionSection)

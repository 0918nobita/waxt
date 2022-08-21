module Waxt.Wasm.Program

open System.IO

let writeHeader (writer: BinaryWriter) =
    writer.Write [| 0x00uy
                    0x61uy
                    0x73uy
                    0x6Duy |] // magic

    writer.Write 1u // version

let () =
    printfn "%B" -1234

    Leb128.singedLeb128 -1234
    |> List.map (sprintf "%07B")
    |> printfn "%A"

    // -1234 の signed LEB128
    // 10101110 01110110
    // 10101110 11110110 01111111
    // 10101110 11110110 11111111 01111111
    // 10101110 11110110 11111111 11111111 01111111

    use file = File.Open("out.wasm", FileMode.Create)
    use writer = new BinaryWriter(file)
    writeHeader writer

    let typeSection = TypeSection(Vec [ FunctionType(Vec [ I32 ], I32) ])
    writer.Write(Section.toBytes typeSection)

    let functionSection = FunctionSection(Vec [])
    writer.Write(Section.toBytes functionSection)

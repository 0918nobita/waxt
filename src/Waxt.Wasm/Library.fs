[<AutoOpen>]
module Waxt.Wasm.Library

open System

type Wasm = Wasm of typeSection: TypeSection * functionSection: FunctionSection * codeSection: CodeSection

module Wasm =
    let toBytes (Wasm (typeSection, functionSection, codeSection)) : list<byte> =
        let magic = [ 0x00uy; 0x61uy; 0x73uy; 0x6Duy ]
        let version = BitConverter.GetBytes 1u |> List.ofArray
        let typeSection = Section.toBytes typeSection
        let functionSection = Section.toBytes functionSection
        let codeSection = Section.toBytes codeSection
        let memorySection = Section.toBytes (MemorySection(Vec [ MemType 1u ]))

        magic
        @ version
          @ typeSection
            @ functionSection @ memorySection @ codeSection

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

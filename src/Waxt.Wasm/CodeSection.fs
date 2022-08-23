[<AutoOpen>]
module Waxt.Wasm.CodeSection

open Leb128

type Locals =
    | Locals of count: uint32 * ``type``: NumberType

    interface ISerializable with
        member this.Serialize() =
            match this with
            | Locals (count, ``type``) ->
                unsignedLeb128 count
                @ (``type`` :> ISerializable).Serialize()

type Inst =
    | I32Const of n: int
    | I32Add
    | I32Mul
    | I32Store
    | LocalGet of index: uint32

module Inst =
    let toBytes (inst: Inst) : list<byte> =
        match inst with
        | I32Const n -> 0x41uy :: signedLeb128 n
        | I32Add -> [ 0x6Auy ]
        | I32Mul -> [ 0x6Cuy ]
        | I32Store -> 0x36uy :: unsignedLeb128 2u @ unsignedLeb128 0u
        | LocalGet index -> 0x20uy :: unsignedLeb128 index

type Expr = Expr of insts: list<Inst>

module Expr =
    let toBytes (expr: Expr) : list<byte> =
        match expr with
        | Expr insts -> (insts |> List.collect Inst.toBytes) @ [ 0x0Buy ]

type Func =
    | Func of locals: Vector<Locals> * expr: Expr

    interface ISerializable with
        member this.Serialize() =
            match this with
            | Func (locals, expr) ->
                (locals :> ISerializable).Serialize()
                @ (Expr.toBytes expr)

type Code =
    | Code of func: Func

    interface ISerializable with
        member this.Serialize() =
            match this with
            | Code funcs ->
                let func = (funcs :> ISerializable).Serialize()
                let size = func |> List.length |> uint32 |> unsignedLeb128
                size @ func

type CodeSection =
    | CodeSection of Vector<Code>

    interface ISection with
        member _.Id = 10uy

        member this.GetContents() =
            match this with
            | CodeSection code -> (code :> ISerializable).Serialize()

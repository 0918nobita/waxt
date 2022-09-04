namespace WAXT.Token

open WAXT.Location

type I32AddOp =
    | I32AddOp of Pos

    interface ILocatable with
        member this.Locate() =
            match this with
            | I32AddOp pos -> Range.fromPos pos

type I32SubOp =
    | I32SubOp of Pos

    interface ILocatable with
        member this.Locate() =
            match this with
            | I32SubOp pos -> Range.fromPos pos

type I32MulOp =
    | I32MulOp of Pos

    interface ILocatable with
        member this.Locate() =
            match this with
            | I32MulOp pos -> Range.fromPos pos

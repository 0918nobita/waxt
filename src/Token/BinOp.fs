namespace WAXT.Token

open WAXT.Location

type I32AddOp =
    | I32AddOp of Pos

module I32AddOp =
    let locate (I32AddOp pos) =
        Range.fromPos pos

type I32SubOp =
    | I32SubOp of Pos

module I32SubOp =
    let locate (I32SubOp pos) =
        Range.fromPos pos

type I32MulOp =
    | I32MulOp of Pos

module I32MulOp =
    let locate (I32MulOp pos) =
        Range.fromPos pos

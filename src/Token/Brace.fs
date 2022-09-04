namespace WAXT.Token

open WAXT.Location

type OpenBrace =
    | OpenBrace of Pos

    interface ILocatable with
        member this.Locate() =
            match this with
            | OpenBrace pos -> Range.fromPos pos

type CloseBrace =
    | CloseBrace of Pos

    interface ILocatable with
        member this.Locate() =
            match this with
            | CloseBrace pos -> Range.fromPos pos

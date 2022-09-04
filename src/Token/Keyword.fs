namespace WAXT.Token

open WAXT.Location

type IfKeyword =
    | IfKeyword of Range

    interface ILocatable with
        member this.Locate() =
            match this with
            | IfKeyword range -> range

[<AutoOpen>]
module Waxt.Location.Locatable

/// ソースファイル上の位置を特定できることを表す
type ILocatable =
    abstract member Locate: unit -> Range

namespace WAXT.Location

type ILocatable =
    abstract member Locate: unit -> Range

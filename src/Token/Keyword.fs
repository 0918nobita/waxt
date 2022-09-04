namespace WAXT.Token

open WAXT.Location

type IfKeyword = IfKeyword of Range

module IfKeyword =
    let locate (IfKeyword range) = range

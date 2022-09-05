namespace Waxt.Token

open Waxt.Location

type IfKeyword = IfKeyword of Range

module IfKeyword =
    let locate (IfKeyword range) = range

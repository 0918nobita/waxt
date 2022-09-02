namespace WAXT.AST

open Thoth.Json.Net

type FuncName =
    private
    | FuncName of string

    override this.ToString() =
        match this with
        | FuncName name -> name

module FuncName =
    let make name = FuncName name

    let toJson (FuncName name) = Encode.string name

module Waxt.TypeInferrer.FuncContext

open Type

type FuncName =
    private
    | FuncName of string

    override this.ToString() =
        match this with
        | FuncName name -> name

module FuncName =
    let make name = FuncName name

type FuncContext = private FuncContext of Map<string, FuncType>

module FuncContext =
    let empty = FuncContext Map.empty

    let add (FuncName name) (funcTy: FuncType) (FuncContext context) =
        context |> Map.add name funcTy |> FuncContext

    let tryFind (FuncName name) (FuncContext context) = context |> Map.tryFind name

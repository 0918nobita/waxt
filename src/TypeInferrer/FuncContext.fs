namespace Waxt.TypeInferrer

open Waxt.Token
open Waxt.Type

type FuncContext = private FuncContext of Map<Ident, FuncType>

module FuncContext =
    let empty = FuncContext Map.empty

    let add (funcName: Ident) (funcTy: FuncType) (FuncContext context) =
        context |> Map.add funcName funcTy |> FuncContext

    let tryFind (funcName: Ident) (FuncContext context) = context |> Map.tryFind funcName

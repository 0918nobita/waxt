namespace WAXT.TypeInferrer

open WAXT.UntypedAst

type FuncContext = private FuncContext of Map<FuncName, FuncType>

module FuncContext =
    let empty = FuncContext Map.empty

    let add (funcName: FuncName) (funcTy: FuncType) (FuncContext context) =
        context |> Map.add funcName funcTy |> FuncContext

    let tryFind (funcName: FuncName) (FuncContext context) = context |> Map.tryFind funcName

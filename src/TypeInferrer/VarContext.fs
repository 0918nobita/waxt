namespace WAXT.TypeInferrer

open WAXT.AST
open WAXT.Type

type VarContext = private VarContext of list<VarName * Type>

module VarContext =
    let empty = VarContext []

    let add (varName: VarName) (ty: Type) (VarContext context) = VarContext((varName, ty) :: context)

    let tryFind (varName: VarName) (VarContext context) : option<Type> =
        context
        |> List.tryFind (fun (varName', _) -> varName' = varName)
        |> Option.map snd

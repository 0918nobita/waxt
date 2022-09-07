namespace Waxt.TypeInferrer

open Waxt.Token
open Waxt.Type

type VarContext = private VarContext of list<Ident * Type>

module VarContext =
    let empty = VarContext []

    let add (varName: Ident) (ty: Type) (VarContext context) = VarContext((varName, ty) :: context)

    let tryFind (varName: Ident) (VarContext context) : option<Type> =
        context
        |> List.tryFind (fun (varName', _) -> varName' = varName)
        |> Option.map snd

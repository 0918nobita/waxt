module Waxt.TypeInferrer.VarContext

open Type

type VarName =
    private
    | VarName of string

    override this.ToString() =
        match this with
        | VarName name -> name

module VarName =
    let make name = VarName name

type VarContext = private VarContext of list<string * Type>

module VarContext =
    let empty = VarContext []

    let add (VarName name) (ty: Type) (VarContext context) = VarContext((name, ty) :: context)

    let tryFind (VarName name) (VarContext context) : option<Type> =
        context
        |> List.tryFind (fun (name', _) -> name' = name)
        |> Option.map snd

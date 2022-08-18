[<AutoOpen>]
module Waxt.Type.TypeEnv

type TypeEnv = private TypeEnv of typeVars: list<string * Type>

module TypeEnv =
    let empty = TypeEnv []

    let ofList typeVars = TypeEnv typeVars

    let add (name: string) (ty: Type) (TypeEnv typeEnv) = TypeEnv((name, ty) :: typeEnv)

    let find (name: string) (TypeEnv typeEnv) =
        typeEnv
        |> List.tryFind (fun (name', _) -> name = name')
        |> Option.map snd

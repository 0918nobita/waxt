[<AutoOpen>]
module Waxt.Type.TypeEnv

type TypeEnv = private TypeEnv of typeVars: list<string * Type>

module TypeEnv =
    let empty = TypeEnv []

    let ofSeq typeVars = TypeEnv(List.ofSeq typeVars)

    let add (name: string) (ty: Type) (TypeEnv typeEnv) = TypeEnv((name, ty) :: typeEnv)

    let find (name: string) (TypeEnv typeEnv) =
        typeEnv
        |> List.tryFind (fun (name', _) -> name = name')
        |> Option.map snd

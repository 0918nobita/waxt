[<AutoOpen>]
module Waxt.Type.TypeEnv

open Waxt.Location

type TypeEnv = private TypeEnv of typeVars: list<(string * Range) * (Type * Range)>

module TypeEnv =
    let empty = TypeEnv []

    let ofSeq typeVars = TypeEnv(List.ofSeq typeVars)

    let add (name: string * Range) (ty: Type * Range) (TypeEnv typeEnv) = TypeEnv((name, ty) :: typeEnv)

    let find (name: string) (TypeEnv typeEnv) =
        typeEnv
        |> List.tryFind (fun ((name', _), _) -> name = name')
        |> Option.map snd

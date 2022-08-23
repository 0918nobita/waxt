[<AutoOpen>]
module Waxt.Type.TypeEnv

open FsToolkit.ErrorHandling
open Waxt.Location

type TypeEnv = private TypeEnv of typeVars: list<(string * Range) * (Type * Range)>

module TypeEnv =
    let empty = TypeEnv []

    let add (name: string * Range) (ty: Type * Range) (TypeEnv typeEnv) = TypeEnv((name, ty) :: typeEnv)

    let find (name: string) (TypeEnv typeEnv) : option<int * (Type * Range)> =
        option {
            let len = List.length typeEnv
            let! index = List.tryFindIndexBack (fun ((name', _), _) -> name = name') typeEnv
            return (len - index - 1, snd typeEnv.[index])
        }

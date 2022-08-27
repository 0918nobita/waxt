[<AutoOpen>]
module Waxt.TypeInferrer.Context

type Context = private Context of list<string * Type>

module Context =
    let empty = Context []

    let add (name: string) (ty: Type) (Context context) = Context((name, ty) :: context)

    let tryFind (name: string) (Context context) : option<Type> =
        context
        |> List.tryFind (fun (name', ty) -> name' = name)
        |> Option.map snd

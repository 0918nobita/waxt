module Waxt.TypeInferrer.TypeEquation

open Type

type TypeSimulEquation =
    private
    | TypeSimulEquation of Set<Type * Type>

    override this.ToString() =
        match this with
        | TypeSimulEquation equations ->
            equations
            |> Seq.map (fun (ty1, ty2) -> $"%O{ty1} = %O{ty2}")
            |> String.concat ", "
            |> sprintf "{ %s }"

module TypeSimulEquation =
    let empty = TypeSimulEquation Set.empty

    let addEquation (lhs: Type) (rhs: Type) (TypeSimulEquation equationSet) =
        TypeSimulEquation(Set.add (lhs, rhs) equationSet)

    let combine (TypeSimulEquation a) (TypeSimulEquation b) = TypeSimulEquation(Set.union a b)

    let combineMany (equations: seq<TypeSimulEquation>) =
        equations
        |> Seq.map (fun (TypeSimulEquation e) -> e)
        |> Set.unionMany
        |> TypeSimulEquation

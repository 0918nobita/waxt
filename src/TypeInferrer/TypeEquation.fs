module Waxt.TypeInferrer.TypeEquation

open System.Collections
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

    interface IEnumerable with
        member this.GetEnumerator() =
            match this with
            | TypeSimulEquation equations -> (equations :> IEnumerable).GetEnumerator()

    interface Generic.IEnumerable<Type * Type> with
        member this.GetEnumerator() =
            match this with
            | TypeSimulEquation equations ->
                (equations :> Generic.IEnumerable<Type * Type>)
                    .GetEnumerator()

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

let assign (tyVarName: TyVarName) (toTy: Type) (equations: list<Type * Type>) =
    equations
    |> List.map (fun (name, ty) -> (name, Type.assign tyVarName toTy ty))

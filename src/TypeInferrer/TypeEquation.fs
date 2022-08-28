module Waxt.TypeInferrer.TypeEquation

open System.Collections
open Type

type TypeEquation =
    | TypeEquation of lhs: Type * rhs: Type

    override this.ToString() =
        match this with
        | TypeEquation (lhs, rhs) -> $"%O{lhs} = %O{rhs}"

type TypeSimulEquation =
    private
    | TypeSimulEquation of Set<TypeEquation>

    override this.ToString() =
        match this with
        | TypeSimulEquation equations ->
            equations
            |> Seq.map string
            |> String.concat ", "
            |> sprintf "{ %s }"

    interface IEnumerable with
        member this.GetEnumerator() =
            match this with
            | TypeSimulEquation equations -> (equations :> IEnumerable).GetEnumerator()

    interface Generic.IEnumerable<TypeEquation> with
        member this.GetEnumerator() =
            match this with
            | TypeSimulEquation equations ->
                (equations :> Generic.IEnumerable<TypeEquation>)
                    .GetEnumerator()

module TypeSimulEquation =
    let empty = TypeSimulEquation Set.empty

    let addEquation (lhs: Type) (rhs: Type) (TypeSimulEquation equationSet) =
        TypeSimulEquation(Set.add (TypeEquation(lhs, rhs)) equationSet)

    let combine (TypeSimulEquation a) (TypeSimulEquation b) = TypeSimulEquation(Set.union a b)

    let combineMany (equations: seq<TypeSimulEquation>) =
        equations
        |> Seq.map (fun (TypeSimulEquation e) -> e)
        |> Set.unionMany
        |> TypeSimulEquation

let assign (tyVarName: TyVarName) (toTy: Type) (equations: list<TypeEquation>) : list<TypeEquation> =
    equations
    |> List.map (fun (TypeEquation (name, ty)) -> TypeEquation(name, Type.assign tyVarName toTy ty))

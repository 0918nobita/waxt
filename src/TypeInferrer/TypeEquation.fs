module Waxt.TypeInferrer.TypeEquation

open System
open System.Collections
open Waxt.Location
open Waxt.Type

[<CustomEquality; CustomComparison>]
type TypeEquation =
    | TypeEquation of lhs: Type * rhs: Type * at: Range

    override this.ToString() =
        match this with
        | TypeEquation (lhs, rhs, at) -> $"%O{lhs} = %O{rhs}"

    override this.Equals(other) =
        match other with
        | :? TypeEquation as (TypeEquation (lhs', rhs', _)) ->
            match this with
            | TypeEquation (lhs, rhs, _) -> lhs = lhs' && rhs = rhs'
        | _ -> false

    override this.GetHashCode() =
        match this with
        | TypeEquation (lhs, rhs, _) -> (lhs, rhs).GetHashCode()

    interface IComparable with
        member this.CompareTo(other) =
            match other with
            | :? TypeEquation as (TypeEquation (lhs', rhs', _)) ->
                match this with
                | TypeEquation (lhs, rhs, _) ->
                    ((lhs, rhs) :> IComparable)
                        .CompareTo((lhs', rhs'))
            | _ -> -1

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

    let add (lhs: Type) (rhs: Type) (at: Range) (TypeSimulEquation equationSet) =
        TypeSimulEquation(Set.add (TypeEquation(lhs, rhs, at)) equationSet)

    let combine (TypeSimulEquation a) (TypeSimulEquation b) = TypeSimulEquation(Set.union a b)

    let combineMany (equations: seq<TypeSimulEquation>) =
        equations
        |> Seq.map (fun (TypeSimulEquation e) -> e)
        |> Set.unionMany
        |> TypeSimulEquation

let assign (tyVarName: TyVarName) (toTy: Type) (equations: list<TypeEquation>) : list<TypeEquation> =
    equations
    |> List.map (fun (TypeEquation (name, ty, at)) -> TypeEquation(name, Type.assign tyVarName toTy ty, at))

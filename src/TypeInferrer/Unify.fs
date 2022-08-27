[<AutoOpen>]
module Waxt.TypeInferrer.Unify

type Assign = Assign of varName: string * ty: Type

let rec unify (equation: list<Type * Type>) : Result<list<Assign>, string> =
    match equation with
    | [] -> Ok []
    | (ty1, ty2) :: rest when ty1 = ty2 -> unify rest
    | ((TyVar name, ty)
    | (ty, TyVar name)) :: rest -> failwith "not implemented"
    | _ -> failwith "not implemented"

[<AutoOpen>]
module Waxt.UntypedAst.Stmt

open Thoth.Json.Net

type Stmt = FuncDef of name: string * result: Type * parameters: list<string * Type> * body: list<Expr>

module Stmt =
    let toJson: Stmt -> JsonValue =
        function
        | FuncDef (name, result, parameters, body) ->
            let parameters =
                parameters
                |> List.map (fun (name, ty) ->
                    Encode.object [ "name", Encode.string name
                                    "ty", Type.toJson ty ])
                |> Encode.list

            let body = body |> List.map Expr.toJson |> Encode.list

            Encode.object (
                [ "type", Encode.string "funcDef"
                  "name", Encode.string name
                  "result", Type.toJson result
                  "parameters", parameters
                  "body", body ]
            )

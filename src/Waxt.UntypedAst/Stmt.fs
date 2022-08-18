[<AutoOpen>]
module Waxt.UntypedAst.Stmt

open Thoth.Json.Net
open Waxt.Type

type Stmt = FuncDefStmt of FuncDef

module Stmt =
    let toJson: Stmt -> JsonValue =
        function
        | FuncDefStmt (FuncDef (name, resultTy, parameters, body)) ->
            let parameters =
                parameters
                |> List.map (fun (name, ty) ->
                    Encode.object [ "name", Encode.string name
                                    "ty", Type.toJson ty ])
                |> Encode.list

            let body = body |> List.map Expr.toJson |> Encode.list

            Encode.object (
                [ "type", Encode.string "funcDef"
                  "name", FuncName.toJson name
                  "result", Type.toJson resultTy
                  "parameters", parameters
                  "body", body ]
            )

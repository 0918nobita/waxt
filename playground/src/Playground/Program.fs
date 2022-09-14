module Waxt.Playground.Program

open Waxt.Lexer
open Waxt.Location

let lexicalAnalysis src =
    match lex (LexOption Lf) src with
    | Ok _ -> [||]
    | Error err ->
        err
        |> List.map (fun (LexError (msg, at)) -> (msg, Pos.line at, Pos.column at))
        |> Array.ofList

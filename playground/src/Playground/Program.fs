module Waxt.Playground.Program

open Waxt.Lexer

let lexicalAnalysis src =
    match lex (LexOption Lf) src with
    | Ok tokens -> printfn "%A" tokens
    | Error err -> eprintfn "%A " err

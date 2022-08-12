module Waxt.Compiler.Program

open FsToolkit.ErrorHandling
open Location

let compile src =
    let tokens = Lex.lex src

    result {
        let! (sExpr, _rest) = ParseSExpr.parseSExpr (Point(0u, 0u)) tokens

        return! ParseStmt.parseStmt sExpr
    }

open System
open System.IO

let error (msg: string) =
    Console.ForegroundColor <- ConsoleColor.Red
    Console.Error.WriteLine msg
    Console.ResetColor()

[<EntryPoint>]
let main args =
    if Array.isEmpty args then
        eprintfn "Usage: waxt <file>"
        error "No input files"
        1
    else
        let inputFilePath = args.[0]

        let src =
            try
                File.ReadAllText inputFilePath
            with
            | :? FileNotFoundException ->
                error "The specified file doesn't exist"
                exit 1

            | :? DirectoryNotFoundException ->
                error "The specified directory doesn't exist"
                exit 1

            | :? IOException ->
                error "Could not read the specified file"
                exit 1

        src |> compile |> printfn "%A"

        0

module Waxt.Cli

open System
open System.IO
open Waxt.Compiler

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

        match compile src with
        | Ok stmts ->
            printfn "%A" stmts
            0

        | Error compileErrors ->
            compileErrors
            |> List.iter (fun err -> error (CompileError.toString err))

            1

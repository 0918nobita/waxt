[<AutoOpen>]
module Waxt.Compiler.Library

open FsToolkit.ErrorHandling
open Waxt.CodeGen
open Waxt.Lexer
open Waxt.Location
open Waxt.Parser
open Waxt.TypeChecker
open Waxt.UntypedAst
open Waxt.Wasm

type CompileError =
    private
    | FromLexer of LexError
    | FromParser of ParseError
    | FromTypeChecker of TypeError

module CompileError =
    let toString =
        function
        | FromLexer lexError -> LexError.toString lexError
        | FromParser parseError -> ParseError.toString parseError
        | FromTypeChecker typeError -> TypeError.toString typeError

let compile src : Result<list<byte>, list<CompileError>> =
    result {
        let! tokens =
            lex src
            |> Result.mapError (fun lexerError -> [ FromLexer lexerError ])

        let! (sExprs, rest) =
            ParseSExpr.parseManySExpr Pos.origin tokens
            |> Result.mapError (fun parseError -> [ FromParser parseError ])

        match rest with
        | [] -> ()
        | tok :: _ -> return! Error [ FromParser(ParseError("Syntax error", (tok :> ILocatable).Locate())) ]

        let! stmts =
            sExprs
            |> List.map (
                ParseUntypedAst.parseStmt
                >> Result.mapError FromParser
            )
            |> List.sequenceResultA

        let funcDefs =
            stmts
            |> List.map (fun (FuncDefStmt funcDef) -> funcDef)

        let! typedFuncs =
            typeFuncDefs funcDefs
            |> Result.mapError (fun typeError -> [ FromTypeChecker typeError ])

        let wasm = genCode typedFuncs

        return Wasm.toBytes wasm
    }

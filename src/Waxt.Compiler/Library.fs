[<AutoOpen>]
module Waxt.Compiler.Library

open FsToolkit.ErrorHandling
open Waxt.Lexer
open Waxt.Location
open Waxt.Parser
open Waxt.UntypedAst

type CompileError =
    private
    | FromLexer of LexError
    | FromParser of ParseError

module CompileError =
    let toString =
        function
        | FromLexer lexError -> LexError.toString lexError
        | FromParser parseError -> ParseError.toString parseError

let compile src : Result<list<Stmt>, list<CompileError>> =
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

        return!
            sExprs
            |> List.map (
                ParseUntypedAst.parseStmt
                >> Result.mapError FromParser
            )
            |> List.sequenceResultA
    }

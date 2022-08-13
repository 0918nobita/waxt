[<AutoOpen>]
module Waxt.Compiler.Library

open FsToolkit.ErrorHandling
open Location

let compile src =
    let tokens = Lex.lex src

    result {
        let! (sExpr, _rest) = ParseSExpr.parseSExpr (Pos(0u, 0u)) tokens

        return! ParseStmt.parseStmt sExpr
    }

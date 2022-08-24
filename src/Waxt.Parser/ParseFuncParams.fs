module Waxt.Parser.ParseFuncParams

open FsToolkit.ErrorHandling
open Waxt.Location
open Waxt.Type

let rec parseFuncParams: list<SExpr> -> ParseResult<list<(string * Range) * option<Type * Range>>> =
    function
    | [] -> Ok []

    | Atom (param, at) :: rest ->
        result {
            let! parameters = parseFuncParams rest
            return ((param, at), None) :: parameters
        }

    | BracketList ([], at) :: _ ->
        Error(ParseError("Expected parameter name or [name : type], but reached last element of list", at))

    | BracketList ([ Atom (param, paramRange); Atom (":", _); Atom (ty, tyRange) ], _) :: rest ->
        result {
            let! ty = ParseType.parseType tyRange ty
            let! parameters = parseFuncParams rest

            return
                ((param, paramRange), Some(ty, tyRange))
                :: parameters
        }

    | BracketList (_, at) :: _
    | ParenList (_, at) :: _ -> Error(ParseError("Expected [name : type]", at))

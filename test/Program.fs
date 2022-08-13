module Waxt.Compiler.Test.Program

open Expecto
open Waxt.Compiler

[<Tests>]
let funcDefTest =
    test "funcDef" {
        let result = compile "(func foo i32 [x i32 y i32] (i32.mul (i32.add 1 2) 4))"
        let expected = Ok(Stmt.FuncDef("foo", Some Stmt.I32, [], []))
        Expect.equal result expected "(func ...) should be compiled to FuncDef"

        let result =
            compile "(func add-and-store [addr *i32 x i32 y i32] (i32.store addr (i32.add x y)))"

        let expected = Ok(Stmt.FuncDef("add-and-store", None, [], []))
        Expect.equal result expected "The result type name in (func ...) can be omitted"
    }

[<EntryPoint>]
let main argv = runTestsInAssembly defaultConfig argv

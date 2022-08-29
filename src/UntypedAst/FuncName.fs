namespace Waxt.UntypedAst

type FuncName =
    private
    | FuncName of string

    override this.ToString() =
        match this with
        | FuncName name -> name

module FuncName =
    let make name = FuncName name

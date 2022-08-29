namespace WAXT.UntypedAst

type VarName =
    private
    | VarName of string

    override this.ToString() =
        match this with
        | VarName name -> name

module VarName =
    let make name = VarName name

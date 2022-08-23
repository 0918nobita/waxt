module Waxt.TypeChecker.SeqExt

let iterWhileOk (f: 't -> Result<unit, 'e>) (sequence: seq<'t>) : Result<unit, 'e> =
    let folder (state: Result<unit, 'e>) (item: 't) = state |> Result.bind (fun () -> f item)

    sequence |> Seq.fold folder (Ok())

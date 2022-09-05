namespace Waxt.TypeInferrer

open Waxt.Location

type TypeInferenceError = TypeInferenceError of msg: string * at: Range

namespace WAXT.TypeInferrer

open WAXT.Location

type TypeInferenceError = TypeInferenceError of msg: string * at: Range

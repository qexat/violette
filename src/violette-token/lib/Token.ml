open Ansifmt

type t =
  { ty : Token_type.t
  ; position : int
  ; length : int
  }

let repr { ty; position; length } : Fmt.t =
  Repr.record
    "Token"
    [ ("ty", Token_type.repr ty)
    ; ("position", Repr.int position)
    ; ("length", Repr.int length)
    ]

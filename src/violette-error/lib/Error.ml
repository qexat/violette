open Ansifmt

type t =
  { ty : Type.t
  ; span : Span.t
  }

let repr { ty; span } : Fmt.t =
  Repr.record
    "Error"
    [ ("ty", Type.repr ty); ("span", Span.repr span) ]

module Type = Type

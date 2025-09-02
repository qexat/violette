open Ext

type t = Leading_underscores_are_reserved

let repr (_ : t) : Fmt.t = Repr.opaque "info"

let synopsis (info : t) : Fmt.t =
  match info with
  | Leading_underscores_are_reserved ->
    Repr.text
      "leading underscores are reserved for standalone \
       operator identifiers"

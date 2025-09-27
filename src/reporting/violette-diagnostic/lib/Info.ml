type t = Leading_underscores_are_reserved

let repr (_ : t) : Better_fmt.t = Repr.opaque "info"

let synopsis (info : t) : Better_fmt.t =
  match info with
  | Leading_underscores_are_reserved ->
    Repr.text
      "leading underscores are reserved for standalone \
       operator identifiers"

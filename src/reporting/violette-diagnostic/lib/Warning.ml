type t = |

let repr (_ : t) : Better_fmt.t = Repr.opaque "warning"

let synopsis (warning : t) : Better_fmt.t =
  match warning with
  | _ -> .

let get_notes (warning : t) : Info.t list =
  match warning with
  | _ -> []

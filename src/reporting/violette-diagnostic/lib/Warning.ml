open Ext

type t = |

let repr (_ : t) : Fmt.t = Repr.opaque "warning"

let synopsis (warning : t) : Fmt.t =
  match warning with
  | _ -> .

let get_notes (warning : t) : Info.t list =
  match warning with
  | _ -> []

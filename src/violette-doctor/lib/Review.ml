open Ext

type t =
  { decision : [ `Pass | `Abort ]
  ; details : Fmt.t list
  }

let create
      (decision : [ `Pass | `Abort ])
      (details : Fmt.t list)
  : t
  =
  { decision; details }

let requires_abortion (diagnostic : Diagnostic.t) : bool =
  match diagnostic.diagnosis with
  | Error _ -> true
  | Warning _ | Info _ -> false

let decide (diagnostics : Diagnostic.t list)
  : [ `Pass | `Abort ]
  =
  if List.exists requires_abortion diagnostics
  then `Abort
  else `Pass

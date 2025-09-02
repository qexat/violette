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

let concat_decision
      (left : [ `Pass | `Abort ])
      (right : [ `Pass | `Abort ])
  : [ `Pass | `Abort ]
  =
  match (left, right) with
  | (`Pass, `Pass) -> `Pass
  | (_, _) -> `Abort

let ( <> ) (left : t) (right : t) : t =
  { decision = concat_decision left.decision right.decision
  ; details = left.details @ right.details
  }

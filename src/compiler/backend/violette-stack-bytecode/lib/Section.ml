open Ext

type t =
  { name : string
  ; instructions : Instruction.t list
  }

let repr { name; instructions } : Fmt.t =
  Repr.section name (List.map Instruction.repr instructions)

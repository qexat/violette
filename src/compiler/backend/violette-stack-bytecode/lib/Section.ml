type t =
  { name : string
  ; instructions : Instruction.t list
  }

let repr { name; instructions } : Better_fmt.t =
  Repr.section name (List.map Instruction.repr instructions)

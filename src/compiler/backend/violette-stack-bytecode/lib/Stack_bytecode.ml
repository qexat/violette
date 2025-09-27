type t =
  { directives : Directive_set.t
  ; sections : Section.t list
  }

let repr { directives; sections } : Better_fmt.t =
  Better_fmt.(
    join
      ~on:(raw "\n\n")
      (Directive_set.repr directives
       :: List.map Section.repr sections))

module Directive_set = Directive_set
module Value = Value
module Instruction = Instruction
module Section = Section

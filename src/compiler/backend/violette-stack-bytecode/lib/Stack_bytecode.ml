open Ext

type t =
  { directives : Directive_set.t
  ; sections : Section.t list
  }

let repr { directives; sections } : Fmt.t =
  Fmt.(
    join
      ~on:(raw "\n\n")
      (Directive_set.repr directives
       :: List.map Section.repr sections))

module Directive_set = Directive_set
module Value = Value
module Instruction = Instruction
module Section = Section

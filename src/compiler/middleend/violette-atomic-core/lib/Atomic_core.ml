type t =
  | Atom of atom
  | Let of string * atom * t
  | Let_compute of string * (atom * atom) * t

and atom =
  | Natural of int64
  | Unit
  | Variable of string
  | Function of string * t

let rec repr : t -> Better_fmt.t = function
  | Atom atom -> repr_atom atom
  | Let (name, body, block) ->
    Repr.record
      "Let"
      [ ("name", Repr.string name)
      ; ("body", repr_atom body)
      ; ("next", repr block)
      ]
  | Let_compute (name, (func, arg), block) ->
    Repr.record
      "Let"
      [ ("name", Repr.string name)
      ; ( "application"
        , Repr.tuple [ repr_atom func; repr_atom arg ] )
      ; ("next", repr block)
      ]

and repr_atom : atom -> Better_fmt.t = function
  | Natural value ->
    Repr.record
      "Natural"
      [ ("value", Repr.int (Int64.to_int value)) ]
  | Unit -> Repr.type_name "Unit"
  | Variable name ->
    Repr.record "Variable" [ ("name", Repr.string name) ]
  | Function (param, body) ->
    Repr.record
      "Function"
      [ ("param", Repr.string param); ("body", repr body) ]

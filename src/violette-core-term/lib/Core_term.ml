open Ext

type t =
  | Apply of t * t
  | Block of t list
  | Function of string * t
  | Let of string * t
  | Natural of int64
  | Unit
  | Variable of string

let apply (func : t) (arg : t) : t = Apply (func, arg)

let ( @=> ) (param : string) (body : t) : t =
  Function (param, body)

let rec repr : t -> Fmt.t = function
  | Apply (func, arg) ->
    Repr.record
      "Apply"
      [ ("function", repr func); ("argument", repr arg) ]
  | Block terms ->
    Repr.record
      "Block"
      [ ("terms", Repr.list_field (List.map repr terms)) ]
  | Function (param, body) ->
    Repr.record
      "Function"
      [ ("param", Repr.string param); ("body", repr body) ]
  | Let (name, body) ->
    Repr.record
      "Let"
      [ ("name", Repr.string name); ("body", repr body) ]
  | Natural value ->
    Repr.record
      "Natural"
      [ ("value", Repr.int (Int64.to_int value)) ]
  | Unit -> Repr.type_name "Unit"
  | Variable name ->
    Repr.record "Variable" [ ("name", Repr.string name) ]

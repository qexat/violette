open Ansifmt
open Ext

type t =
  | Closure of t Env.t * string * Core_term.t
  | Natural of int64
  | Unit

let repr : t -> Fmt.t = function
  | Closure _ -> Repr.opaque "fun"
  | Natural value -> Repr.numeric (Int64.to_string value)
  | Unit -> Repr.operator "()"

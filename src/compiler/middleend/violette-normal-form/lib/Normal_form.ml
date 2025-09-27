open Ext

type 'term t =
  | Closure of 'term t Env.t * string * 'term
  | Natural of int64
  | Unit

let repr : type term. term t -> Better_fmt.t = function
  | Closure _ -> Repr.opaque "fun"
  | Natural value -> Repr.numeric (Int64.to_string value)
  | Unit -> Repr.operator "()"

module type TYPE = sig
  type t
end

module Make (T : TYPE) = struct
  type nonrec t = T.t t

  let repr : t -> Better_fmt.t = repr
end

open Ext

type t =
  | Natural of int64
  | Unit

let repr : t -> Fmt.t = function
  | Natural value -> Repr.int (Int64.to_int value)
  | Unit -> Repr.tuple []

type t =
  | Natural of int64
  | Unit

let repr : t -> Better_fmt.t = function
  | Natural value -> Repr.int (Int64.to_int value)
  | Unit -> Repr.tuple []

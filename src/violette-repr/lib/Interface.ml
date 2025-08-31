open Ansifmt

module type TYPE_0 = sig
  type t

  val repr : t -> Fmt.t
end

module type TYPE_1 = sig
  type 'a t

  val repr : ('a -> Fmt.t) -> 'a t -> Fmt.t
end

module type TYPE_2 = sig
  type ('a, 'b) t

  val repr
    :  ('a -> Fmt.t)
    -> ('b -> Fmt.t)
    -> ('a, 'b) t
    -> Fmt.t
end

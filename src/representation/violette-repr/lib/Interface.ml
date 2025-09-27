module type TYPE_0 = sig
  type t

  val repr : t -> Better_fmt.t
end

module type TYPE_1 = sig
  type 'a t

  val repr : ('a -> Better_fmt.t) -> 'a t -> Better_fmt.t
end

module type TYPE_2 = sig
  type ('a, 'b) t

  val repr
    :  ('a -> Better_fmt.t)
    -> ('b -> Better_fmt.t)
    -> ('a, 'b) t
    -> Better_fmt.t
end

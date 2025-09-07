open Ext

type t =
  | Push of Value.t
  | Call
  | Load of string
  | Store of string
  | Return

let repr : t -> Fmt.t = function
  | Push value -> Repr.instruction "PUSH" [ Value.repr value ]
  | Call -> Repr.instruction "CALL" []
  | Load name ->
    Repr.instruction "LOAD" [ Repr.identifier name ]
  | Store name ->
    Repr.instruction "STORE" [ Repr.identifier name ]
  | Return -> Repr.instruction "RETURN" []

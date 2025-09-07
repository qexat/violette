open Ext

type expr =
  (* sorting: by precedence then lexicographically *)
  | Natural of int64
  | Unit
  | Variable of string
  | Block of expr list
  | Apply of expr * expr list
  | Function of string list * expr
  | Let of string * string list * expr

let rec repr : expr -> Fmt.t = function
  | Natural value -> Repr.numeric (Int64.to_string value)
  | Unit -> Repr.tuple []
  | Variable name -> Repr.identifier name
  | Block exprs -> Repr.block (List.map repr exprs)
  | Apply (func, args) ->
    Repr.application (repr func) (List.map repr args)
  | Function (params, body) -> Repr.lambda params (repr body)
  | Let (name, params, body) ->
    Repr.let_definition name ~params (repr body)

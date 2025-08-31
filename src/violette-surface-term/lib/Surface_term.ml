open Ext

type expr =
  (* sorting: by precedence then lexicographically *)
  | Natural of int64
  | Unit
  | Variable of string
  | Block of expr list
  | Apply of expr * expr list
  | Function of string list * expr
  | Let of string * expr

let rec repr : expr -> Fmt.t = function
  | Natural value -> Repr.numeric (Int64.to_string value)
  | Unit -> Repr.operator "()"
  | Variable name -> Repr.identifier name
  | Block exprs -> Repr.block (List.map repr exprs)
  | Apply (func, args) ->
    Repr.application (repr func) (List.map repr args)
  | Function (params, body) -> Repr.func params (repr body)
  | Let (name, body) -> Repr.let_definition name (repr body)

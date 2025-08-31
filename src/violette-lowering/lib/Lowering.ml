let rec surface_to_core (expr : Surface_term.expr) : Core_term.t
  =
  match expr with
  | Apply (func, args) ->
    Curry.curry_left
      func
      args
      ~fixpoint_map:surface_to_core
      ~item_map:surface_to_core
      ~constructor:Core_term.apply
  | Block exprs -> Block (List.map surface_to_core exprs)
  | Function (params, body) ->
    Curry.curry_right
      params
      body
      ~fixpoint_map:surface_to_core
      ~item_map:Fun.id
      ~constructor:Core_term.( @=> )
  | Let (name, body) -> Let (name, surface_to_core body)
  | Natural value -> Natural value
  | Unit -> Unit
  | Variable name -> Variable name

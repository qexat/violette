let make_dummy_identifier =
  Ext.make_dummy_identifier_generator ~prefix:"lc"

let rec lower (expr : Surface_term.expr) : Lambda_core.t =
  match expr with
  | Apply (func, args) ->
    Curry.curry_left
      func
      args
      ~fixpoint_map:lower
      ~item_map:lower
      ~constructor:Lambda_core.apply
  | Block exprs -> lower_surface_block exprs
  | Function (params, body) ->
    Curry.curry_right
      params
      body
      ~fixpoint_map:lower
      ~item_map:Fun.id
      ~constructor:Lambda_core.( @=> )
  | Let (name, [], body) -> Let (name, lower body)
  | Let (name, params, body) ->
    Let (name, lower (Function (params, body)))
  | Natural value -> Natural value
  | Unit -> Unit
  | Variable name -> Variable name

and lower_surface_block (exprs : Surface_term.expr list)
  : Lambda_core.t
  =
  match exprs with
  | [] -> Unit
  | last :: [] -> lower last
  | Let (name, [], body) :: rest ->
    Let_in (name, lower body, lower_surface_block rest)
  | Let (name, params, body) :: rest ->
    Let_in
      ( name
      , lower (Function (params, body))
      , lower_surface_block rest )
  | valued :: rest ->
    Let_in
      ( make_dummy_identifier ()
      , lower valued
      , lower_surface_block rest )

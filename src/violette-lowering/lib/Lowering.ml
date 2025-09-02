let make_index =
  let state = ref 0 in
  fun () ->
    let value = !state in
    incr state;
    value

let make_dummy_identifier () =
  "$" ^ Int.to_string (make_index ())

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
  | Block exprs -> lower_surface_block exprs
  | Function (params, body) ->
    Curry.curry_right
      params
      body
      ~fixpoint_map:surface_to_core
      ~item_map:Fun.id
      ~constructor:Core_term.( @=> )
  | Let (name, body) -> Let (name, surface_to_core body, Unit)
  | Natural value -> Natural value
  | Unit -> Unit
  | Variable name -> Variable name

and lower_surface_block (exprs : Surface_term.expr list)
  : Core_term.t
  =
  match exprs with
  | [] -> Unit
  | last :: [] -> surface_to_core last
  | Let (name, body) :: rest ->
    Let (name, surface_to_core body, lower_surface_block rest)
  | valued :: rest ->
    Let
      ( make_dummy_identifier ()
      , surface_to_core valued
      , lower_surface_block rest )

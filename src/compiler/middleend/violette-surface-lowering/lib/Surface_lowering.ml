let make_index =
  let state = ref 0 in
  fun () ->
    let value = !state in
    incr state;
    value

let make_dummy_identifier () =
  "$" ^ Int.to_string (make_index ())

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
  | Let (name, body) -> Let (name, lower body, Unit)
  | Natural value -> Natural value
  | Unit -> Unit
  | Variable name -> Variable name

and lower_surface_block (exprs : Surface_term.expr list)
  : Lambda_core.t
  =
  match exprs with
  | [] -> Unit
  | last :: [] -> lower last
  | Let (name, body) :: rest ->
    Let (name, lower body, lower_surface_block rest)
  | valued :: rest ->
    Let
      ( make_dummy_identifier ()
      , lower valued
      , lower_surface_block rest )

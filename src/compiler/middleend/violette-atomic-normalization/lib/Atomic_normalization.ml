let make_dummy_identifier () =
  Ext.make_dummy_identifier_generator ~prefix:"ac" ()

let ( let@@ ) = ( @@ )

let rec normalize (term : Lambda_core.t) : Atomic_core.t =
  Fun.flip
    normalize_continuation_passing_style
    (fun value : Atomic_core.t -> Atom value)
    term

and normalize_continuation_passing_style
      (term : Lambda_core.t)
      (continuation : Atomic_core.(atom -> t))
  : Atomic_core.t
  =
  match term with
  | Apply (func, arg) ->
    let@@ func' = normalize_continuation_passing_style func in
    let@@ arg' = normalize_continuation_passing_style arg in
    let result = make_dummy_identifier () in
    Let_compute
      (result, (func', arg'), continuation (Variable result))
  | Function (param, body) ->
    continuation (Function (param, normalize body))
  | Let (name, body) ->
    let@@ body' = normalize_continuation_passing_style body in
    Let (name, body', Atom Unit)
  | Let_in (name, body, block) ->
    let@@ body' = normalize_continuation_passing_style body in
    Let (name, body', normalize block)
  | Natural value -> continuation (Natural value)
  | Unit -> continuation Unit
  | Variable name -> continuation (Variable name)

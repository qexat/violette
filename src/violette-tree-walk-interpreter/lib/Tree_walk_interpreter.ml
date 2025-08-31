open Ansifmt
open Ext

type t =
  { file : File.t
  ; mutable env : Core_term_normal_form.t Env.t
  }

let create ?(env = []) (file : File.t) : t = { file; env }

let fork
      ?(env : Core_term_normal_form.t Env.t option)
      (interpreter : t)
  : t
  =
  { file = interpreter.file
  ; env = Option.value env ~default:interpreter.env
  }

let repr (interpreter : t) : Fmt.t =
  Repr.record
    "Interpreter"
    [ ("file", File.repr interpreter.file)
    ; ("env", Repr.opaque "env")
    ]

let make_error (error_type : Error.Type.t) (interpreter : t)
  : Error.t
  =
  (* TODO: source mapping to get error positions! *)
  { ty = error_type
  ; span = Span.from_offset interpreter.file 0 0
  }

let fetch_variable (name : string) (interpreter : t)
  : (Core_term_normal_form.t, Error.t) result
  =
  Env.fetch
    ~error:(make_error (Unbound_variable name) interpreter)
    name
    interpreter.env

let set_variable
      (name : string)
      (value : Core_term_normal_form.t)
      (interpreter : t)
  =
  interpreter.env <- (name, value) :: interpreter.env

let rec eval (term : Core_term.t) (interpreter : t)
  : (Core_term_normal_form.t, Error.t) result
  =
  match term with
  | Apply (func, arg) ->
    let*! func_normal_form = eval func interpreter in
    let*! arg_normal_form = eval arg interpreter in
    apply_function func_normal_form arg_normal_form interpreter
  | Block terms ->
    let subinterpreter = fork interpreter in
    eval_block_exprs terms subinterpreter
  | Function (param, body) ->
    Ok (Closure (interpreter.env, param, body))
  | Let (name, body) ->
    let*! body_normal_form = eval body interpreter in
    set_variable name body_normal_form interpreter;
    Ok Core_term_normal_form.Unit
  | Natural value -> Ok (Natural value)
  | Unit -> Ok Unit
  | Variable name -> fetch_variable name interpreter

and eval_scoped
      (term : Core_term.t)
      (env : Core_term_normal_form.t Env.t)
      (interpreter : t)
  : (Core_term_normal_form.t, Error.t) result
  =
  let subinterpreter = create ~env interpreter.file in
  eval term subinterpreter

and apply_function
      (func : Core_term_normal_form.t)
      (arg : Core_term_normal_form.t)
      (interpreter : t)
  : (Core_term_normal_form.t, Error.t) result
  =
  match func with
  | Closure (env, param, body) ->
    let subinterpreter = fork ~env interpreter in
    set_variable param arg subinterpreter;
    eval body subinterpreter
  | Natural _ | Unit ->
    Error (make_error (Illegal_application func) interpreter)

and eval_block_exprs
      (exprs : Core_term.t list)
      (interpreter : t)
  : (Core_term_normal_form.t, Error.t) result
  =
  match exprs with
  | [] -> Ok Unit
  | last :: [] -> eval last interpreter
  | first :: rest ->
    let*! _ = eval first interpreter in
    eval_block_exprs rest interpreter

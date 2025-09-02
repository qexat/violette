open Ext
open Diagnostic

type t =
  { doctor : Doctor.t
  ; file : File.t
  ; mutable env : Core_term.t Normal_form_poly.t Env.t
  }

let create ?(env = []) (doctor : Doctor.t) (file : File.t) : t =
  { doctor; file; env }

let fork
      ?with_env:
        (env : Core_term.t Normal_form_poly.t Env.t option)
      (interpreter : t)
  : t
  =
  create
    ~env:(Option.value env ~default:interpreter.env)
    interpreter.doctor
    interpreter.file

let repr (interpreter : t) : Fmt.t =
  Repr.record
    "Interpreter"
    [ ("file", File.repr interpreter.file)
    ; ("env", Repr.opaque "env")
    ]

let add_error (error : Error.t) (interpreter : t) : unit =
  Doctor.add_error
    error
    (Span.from_offset interpreter.file ~-1 ~-1)
    interpreter.doctor

let fetch_variable (name : string) (interpreter : t)
  : Core_term.t Normal_form_poly.t option
  =
  Env.fetch name interpreter.env

let set_variable
      (name : string)
      (value : Core_term.t Normal_form_poly.t)
      (interpreter : t)
  : unit
  =
  interpreter.env <- (name, value) :: interpreter.env

let rec eval (term : Core_term.t) (interpreter : t)
  : Core_term.t Normal_form_poly.t option
  =
  match term with
  | Apply (func, arg) ->
    let*? func_normal_form = eval func interpreter in
    let*? arg_normal_form = eval arg interpreter in
    apply_function func_normal_form arg_normal_form interpreter
  | Function (param, body) ->
    Some (Closure (interpreter.env, param, body))
  | Let (name, body, block) ->
    let*? body_normal_form = eval body interpreter in
    let subinterpreter = fork interpreter in
    set_variable name body_normal_form subinterpreter;
    eval block subinterpreter
  | Natural value -> Some (Natural value)
  | Unit -> Some Unit
  | Variable name -> fetch_variable name interpreter

and eval_scoped
      (term : Core_term.t)
      (env : Core_term.t Normal_form_poly.t Env.t)
      (interpreter : t)
  : Core_term.t Normal_form_poly.t option
  =
  let subinterpreter = fork ~with_env:env interpreter in
  eval term subinterpreter

and apply_function
      (func : Core_term.t Normal_form_poly.t)
      (arg : Core_term.t Normal_form_poly.t)
      (interpreter : t)
  : Core_term.t Normal_form_poly.t option
  =
  match func with
  | Closure (env, param, body) ->
    let subinterpreter = fork ~with_env:env interpreter in
    set_variable param arg subinterpreter;
    eval body subinterpreter
  | Natural _ | Unit ->
    add_error (Illegal_application func) interpreter;
    None

open Ansifmt
open Ext

type t =
  { doctor : Doctor.t
  ; mutable env : Lambda_core.t Normal_form.t Env.t
  ; mutable prompt_in : Better_fmt.t
  ; mutable prompt_out : Better_fmt.t
  ; mutable prompt_err : Better_fmt.t
  ; mutable banner_start : Better_fmt.t
  ; mutable banner_end : Better_fmt.t (* config *)
  ; mutable show_styling : [ `Auto | `Always | `Never ]
  }

type state =
  [ `Listening
  | `Responding
  ]

module Defaults = struct
  open Ansi
  open Better_fmt

  let prompt_in : Better_fmt.t = stylize (raw "<<<") `Bold

  let prompt_out : Better_fmt.t =
    stylize (raw ">>>") (`Foreground Color.blue & `Bold)

  let prompt_err : Better_fmt.t =
    stylize (raw ">>>") (`Foreground Color.red & `Bold)

  let banner_start : Better_fmt.t =
    join
      ~on:(raw " ")
      [ stylize
          (raw "Violette")
          (`Bold & `Foreground Color.magenta)
      ; stylize (raw "--") `Dim
      ; stylize (raw " Ctrl ") `Reverse
      ; stylize (raw " D ") `Reverse
      ; raw "to quit"
      ]

  let banner_end : Better_fmt.t =
    stylize (raw "Goodbye!") (`Foreground Color.yellow)
end

module Output = struct
  type t = Lambda_core.t Normal_form.t Compiler.Output.t
end

let create
      ?(env = [])
      ?(doctor = Doctor.create ())
      ?(prompt_in = Defaults.prompt_in)
      ?(prompt_out = Defaults.prompt_out)
      ?(prompt_err = Defaults.prompt_err)
      ?(banner_start = Defaults.banner_start)
      ?(banner_end = Defaults.banner_end)
      ~(show_styling : [ `Always | `Auto | `Never ])
      ()
  : t
  =
  { env
  ; doctor
  ; prompt_in
  ; prompt_out
  ; prompt_err
  ; banner_start
  ; banner_end
  ; show_styling
  }

let repr
      { env = _
      ; doctor = _
      ; prompt_in
      ; prompt_out
      ; prompt_err
      ; banner_start
      ; banner_end
      ; show_styling = _
      }
  : Better_fmt.t
  =
  Repr.record
    "Toplevel"
    [ ("env", Repr.opaque "env")
    ; ("doctor", Repr.opaque "doctor")
    ; ("prompt_in", prompt_in)
    ; ("prompt_out", prompt_out)
    ; ("prompt_err", prompt_err)
    ; ("banner_start", banner_start)
    ; ("banner_end", banner_end)
    ; ("show_styling", Repr.opaque "option")
    ]

let get_channel : [ `Out | `Err ] -> out_channel = function
  | `Out -> stdout
  | `Err -> stderr

let get_prompt
      (state : state)
      (channel_type : [ `Out | `Err ])
      (toplevel : t)
  : Better_fmt.t
  =
  match state with
  | `Listening -> toplevel.prompt_in
  | `Responding ->
    (match channel_type with
     | `Out -> toplevel.prompt_out
     | `Err -> toplevel.prompt_err)

let print_prompt
      ~(state : state)
      (channel_type : [ `Out | `Err ])
      (toplevel : t)
  =
  let prompt = get_prompt state channel_type toplevel in
  let out = get_channel channel_type in
  Better_fmt.print ~out ~ending:(Some " ") prompt

let listen (toplevel : t) : string option =
  print_prompt ~state:`Listening `Out toplevel;
  match read_line () with
  | exception End_of_file ->
    print_char '\n';
    None
  | source -> Some source

let respond
      ?(channel_type = `Out)
      (message : Better_fmt.t)
      (toplevel : t)
  =
  print_prompt ~state:`Responding channel_type toplevel;
  let out = get_channel channel_type in
  Better_fmt.print ~out message

let print_value
      (value : Lambda_core.t Normal_form.t)
      (toplevel : t)
  : unit
  =
  respond (Normal_form.repr value) toplevel

let report_errors (toplevel : t) : unit =
  let review = Doctor.review ~mode:`Toplevel toplevel.doctor in
  let out = get_channel `Err in
  List.iter
    (fun diagnostic -> Better_fmt.print ~out diagnostic)
    review.details

let compile (file : File.t) (toplevel : t)
  : Lambda_core.t option
  =
  let compiler = Compiler.create toplevel.doctor file in
  let output = Compiler.compile ~target:Lambda_core compiler in
  match output.artifact with
  | None -> None
  | Some (Lambda_core term) -> Some term

let eval (source : string) (toplevel : t)
  : Lambda_core.t Normal_form.t option
  =
  let file = File.create ~path:"<toplevel>" ~contents:source in
  let*? term = compile file toplevel in
  let interpreter =
    Tree_walk_interpreter.create
      ~env:toplevel.env
      toplevel.doctor
      file
  in
  let*? value = Tree_walk_interpreter.eval term interpreter in
  toplevel.env <- interpreter.env;
  Some value

let setup (toplevel : t) : unit =
  Out_channel.set_buffered stderr false;
  Better_fmt.print toplevel.banner_start

let rec loop (toplevel : t) =
  match listen toplevel with
  | None -> ()
  | Some source ->
    (match eval source toplevel with
     | Some value -> print_value value toplevel
     | None -> report_errors toplevel);
    Doctor.clear toplevel.doctor;
    loop toplevel

let teardown (toplevel : t) =
  Better_fmt.print toplevel.banner_end;
  Out_channel.set_buffered stderr true

let run (toplevel : t) =
  setup toplevel;
  loop toplevel;
  teardown toplevel

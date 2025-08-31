open Ansifmt
open Ext

type t =
  { mutable env : Core_term_normal_form.t Env.t
  ; mutable prompt_in : Fmt.t
  ; mutable prompt_out : Fmt.t
  ; mutable prompt_err : Fmt.t
  ; mutable banner_start : Fmt.t
  ; mutable banner_end : Fmt.t
  }

type state =
  [ `Listening
  | `Responding
  ]

module Defaults = struct
  open Ansi
  open Fmt

  let prompt_in : Fmt.t = stylize (raw "<<<") `Bold

  let prompt_out : Fmt.t =
    stylize (raw ">>>") (`Foreground Color.blue & `Bold)

  let prompt_err : Fmt.t =
    stylize (raw ">>>") (`Foreground Color.red & `Bold)

  let banner_start : Fmt.t =
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

  let banner_end : Fmt.t =
    stylize (raw "goodbye!") (`Foreground Color.yellow)
end

let create
      ?(prompt_in = Defaults.prompt_in)
      ?(prompt_out = Defaults.prompt_out)
      ?(prompt_err = Defaults.prompt_err)
      ?(banner_start = Defaults.banner_start)
      ?(banner_end = Defaults.banner_end)
      ?(env = [])
      ()
  : t
  =
  { env
  ; prompt_in
  ; prompt_out
  ; prompt_err
  ; banner_start
  ; banner_end
  }

let repr
      { env = _
      ; prompt_in
      ; prompt_out
      ; prompt_err
      ; banner_start
      ; banner_end
      }
  : Fmt.t
  =
  Repr.record
    "Toplevel"
    [ ("env", Repr.opaque "env")
    ; ("prompt_in", prompt_in)
    ; ("prompt_out", prompt_out)
    ; ("prompt_err", prompt_err)
    ; ("banner_start", banner_start)
    ; ("banner_end", banner_end)
    ]

let get_channel : [ `Out | `Err ] -> out_channel = function
  | `Out -> stdout
  | `Err -> stderr

let print_prompt
      ~(state : state)
      (channel_type : [ `Out | `Err ])
      (toplevel : t)
  =
  let prompt =
    match state with
    | `Listening -> toplevel.prompt_in
    | `Responding -> toplevel.prompt_out
  in
  let out = get_channel channel_type in
  Fmt.print ~out ~ending:(Some " ") prompt

let listen (toplevel : t) : string option =
  print_prompt ~state:`Listening `Out toplevel;
  match read_line () with
  | exception End_of_file ->
    print_char '\n';
    None
  | source -> Some source

let respond
      ?(channel_type = `Out)
      (message : Fmt.t)
      (toplevel : t)
  =
  print_prompt ~state:`Responding channel_type toplevel;
  let out = get_channel channel_type in
  Fmt.print ~out message

let print_value (value : Core_term_normal_form.t) (toplevel : t)
  : unit
  =
  respond (Core_term_normal_form.repr value) toplevel

let print_error (error : Error.t) (toplevel : t) : unit =
  (* todo: properly render the error *)
  respond ~channel_type:`Err (Error.render error) toplevel

let eval (source : string) (toplevel : t)
  : (Core_term_normal_form.t, Error.t) result
  =
  let file = File.create ~path:"<toplevel>" ~contents:source in
  let tokenizer = Tokenizer.create file in
  let*! tokens = Tokenizer.tokenize tokenizer in
  let parser = Parser.create file tokens in
  let*! expr = Parser.parse parser in
  let term = Lowering.surface_to_core expr in
  let interpreter =
    Tree_walk_interpreter.create ~env:toplevel.env file
  in
  let*! value = Tree_walk_interpreter.eval term interpreter in
  toplevel.env <- interpreter.env;
  Ok value

let setup (toplevel : t) : unit =
  Out_channel.set_buffered stderr false;
  Fmt.print toplevel.banner_start

let rec loop (toplevel : t) =
  match listen toplevel with
  | None -> ()
  | Some source ->
    (match eval source toplevel with
     | Ok value -> print_value value toplevel
     | Error error -> print_error error toplevel);
    loop toplevel

let teardown (toplevel : t) =
  respond toplevel.banner_end toplevel;
  Out_channel.set_buffered stderr true

let run (toplevel : t) =
  setup toplevel;
  loop toplevel;
  teardown toplevel

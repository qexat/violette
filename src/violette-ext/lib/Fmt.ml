(* a better version of my own (sub)library :') *)

open Ansifmt

type t =
  | Raw of string
  | Stylized of t * Ansi.t
  | Join of t * t list
  | Section of t

let raw (contents : string) : t = Raw contents

let stylize (fmt : t) (styling : Ansi.t) : t =
  Stylized (fmt, styling)

let join ?(on : t = Raw " ") (fmts : t list) : t =
  Join (on, fmts)

let section (fmt : t) : t = Section fmt
let concat (fmts : t list) = join ~on:(raw "") fmts
let ( ++ ) (left : t) (right : t) : t = concat [ left; right ]

let rec render ~(with_styling : bool) (fmt : t) : string =
  match fmt with
  | Raw contents -> contents
  | Stylized (fmt, ansi) ->
    if with_styling
    then Ansi.wrap ansi (render ~with_styling fmt)
    else render ~with_styling fmt
  | Join (on, fmts) ->
    fmts
    |> List.map (render ~with_styling)
    |> String.concat (render ~with_styling on)
  (* TODO: break lines on sections *)
  | Section fmt -> render ~with_styling fmt

let print
      ?(out = stdout)
      ?(ending = Some "\n")
      ?with_styling:(color_strategy = `Auto)
      (fmt : t)
  : unit
  =
  let with_styling =
    match color_strategy with
    | `Always -> true
    | `Never -> false
    | `Auto -> Out_channel.isatty out
  in
  let ending = Option.value ending ~default:"" in
  Printf.fprintf out "%s%s" (render ~with_styling fmt) ending

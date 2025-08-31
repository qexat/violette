open Ext

type t =
  { ty : Type.t
  ; span : Span.t
  }

let repr { ty; span } : Fmt.t =
  Repr.record
    "Error"
    [ ("ty", Type.repr ty); ("span", Span.repr span) ]

open Ansifmt.Ansi
open Fmt

let render { ty; span = _ } : Fmt.t =
  stylize (raw "error:") (`Foreground Color.red & `Bold)
  ++ raw " "
  ++ Type.synopsis ty

module Type = Type

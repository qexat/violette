open Ext

type t =
  [ `Toplevel
  | `Compiler
  ]

let select_parts (view : t) (parts : Rendered_parts.t)
  : Fmt.t list
  =
  match view with
  | `Toplevel -> parts.header :: parts.notes
  | `Compiler ->
    [ parts.header; parts.file_metadata; parts.contextual_code ]
    @ parts.notes

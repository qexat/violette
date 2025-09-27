type t =
  [ `Toplevel
  | `Compiler
  ]

let select_parts (view : t) (parts : Rendered_parts.t)
  : Better_fmt.t list
  =
  match view with
  | `Toplevel -> parts.header :: parts.notes
  | `Compiler ->
    [ parts.header; parts.file_metadata; parts.contextual_code ]
    @ parts.notes

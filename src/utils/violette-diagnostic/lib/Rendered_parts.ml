open Ext

type t =
  { header : Fmt.t
  ; file_metadata : Fmt.t
  ; contextual_code : Fmt.t
  ; notes : Fmt.t list
  }

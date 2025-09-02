open Ext
open Fmt

type t =
  { diagnosis : Diagnosis.t
  ; span : Span.t
  }

let create (diagnosis : Diagnosis.t) (span : Span.t) : t =
  { diagnosis; span }

let render_parts (diagnostic : t) : Rendered_parts.t =
  { header = Diagnosis.header diagnostic.diagnosis
  ; file_metadata =
      join
        [ Repr.indent
        ; Repr.keyword "in"
        ; Repr.string diagnostic.span.file.path
        ]
  ; contextual_code = Repr.opaque "code (todo)"
  ; notes =
      diagnostic.diagnosis
      |> Diagnosis.get_notes
      |> List.map Diagnosis.info
      |> List.map Diagnosis.header
  }

let render ~(mode : Mode.t) (diagnostic : t) : Fmt.t =
  join_lines (Mode.select_parts mode (render_parts diagnostic))

module Rendered_parts = Rendered_parts
module Mode = Mode
module Diagnosis = Diagnosis
module Error = Error
module Warning = Warning
module Info = Info

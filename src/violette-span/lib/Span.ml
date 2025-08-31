open Ansifmt

type t =
  { file : File.t
  ; start_position : Position.t
  ; end_position : Position.t
  }

let repr { file; start_position; end_position } : Fmt.t =
  Repr.record
    "Span"
    [ ("file", File.repr file)
    ; ("start", Position.repr start_position)
    ; ("end", Position.repr end_position)
    ]

let from_offset
      (file : File.t)
      (start_offset : int)
      (end_offset : int)
  : t
  =
  { file
  ; start_position = Position.from_offset file start_offset
  ; end_position = Position.from_offset file end_offset
  }

module Position = Position

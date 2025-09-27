type t =
  { line : int
  ; column : int
  }

let repr { line; column } : Better_fmt.t =
  Repr.record
    "Position"
    [ ("line", Repr.int line); ("column", Repr.int column) ]

let from_offset (file : File.t) (offset : int) : t =
  let lines_up_to_offset =
    String.split_on_char
      '\n'
      (String.sub file.contents 0 offset)
  in
  let line = List.length lines_up_to_offset in
  { line
  ; column =
      String.length (List.nth lines_up_to_offset (line - 1))
  }

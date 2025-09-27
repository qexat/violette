type t =
  { path : string
  ; contents : string
  }

let create ~(path : string) ~(contents : string) : t =
  { path; contents }

let repr { path : string; contents : string } : Better_fmt.t =
  Repr.record
    "File"
    [ ("path", Repr.string path)
    ; ("contents", Repr.string contents)
    ]

let lines (file : t) : string list =
  String.split_on_char '\n' file.contents

let lines_in_range
      ~from:(start : int)
      ~to_:(end_ : int)
      (file : t)
  : string list
  =
  List.filteri
    (fun index _ -> start <= index + 1 && index + 1 <= end_)
    (lines file)

let enumerate_lines_in_range
      ~from:(start : int)
      ~to_:(end_ : int)
      (file : t)
  : (int * string) list
  =
  List.mapi
    (fun index line -> (start + index + 1, line))
    (lines_in_range ~from:start ~to_:end_ file)

let line (number : int) (file : t) : string option =
  List.nth_opt (lines file) number

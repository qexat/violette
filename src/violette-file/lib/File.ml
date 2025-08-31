type t =
  { path : string
  ; contents : string
  }

let create ~(path : string) ~(contents : string) : t =
  { path; contents }

let repr { path : string; contents : string } : Ansifmt.Fmt.t =
  Repr.record
    "File"
    [ ("path", Repr.string path)
    ; ("contents", Repr.string contents)
    ]

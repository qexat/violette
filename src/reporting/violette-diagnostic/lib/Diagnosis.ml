[@@@warning "-56"]

type t =
  | Error of Error.t
  | Warning of Warning.t
  | Info of Info.t

let error (error : Error.t) : t = Error error
let warning (warning : Warning.t) : t = Warning warning
let info (info : Info.t) : t = Info info

let repr : t -> Better_fmt.t = function
  | Error error -> Error.repr error
  | Warning _ -> .
  | Info info -> Info.repr info

let title (diagnosis : t) : Better_fmt.t =
  match diagnosis with
  | Error _ -> Repr.error "error:"
  | Warning _ -> Repr.warning "warning:"
  | Info _ -> Repr.info "info:"

let synopsis (diagnosis : t) : Better_fmt.t =
  match diagnosis with
  | Error error -> Error.synopsis error
  | Warning warning -> Warning.synopsis warning
  | Info info -> Info.synopsis info

let header (diagnosis : t) : Better_fmt.t =
  Better_fmt.join [ title diagnosis; synopsis diagnosis ]

let get_notes (diagnosis : t) : Info.t list =
  match diagnosis with
  | Error error -> Error.get_notes error
  | Warning warning -> Warning.get_notes warning
  | Info _ -> []

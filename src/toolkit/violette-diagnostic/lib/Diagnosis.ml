[@@@warning "-56"]

open Ext

type t =
  | Error of Error.t
  | Warning of Warning.t
  | Info of Info.t

let error (error : Error.t) : t = Error error
let warning (warning : Warning.t) : t = Warning warning
let info (info : Info.t) : t = Info info

let repr : t -> Fmt.t = function
  | Error error -> Error.repr error
  | Warning _ -> .
  | Info info -> Info.repr info

let title (diagnosis : t) : Fmt.t =
  match diagnosis with
  | Error _ -> Repr.error "error:"
  | Warning _ -> Repr.warning "warning:"
  | Info _ -> Repr.info "info:"

let synopsis (diagnosis : t) : Fmt.t =
  match diagnosis with
  | Error error -> Error.synopsis error
  | Warning warning -> Warning.synopsis warning
  | Info info -> Info.synopsis info

let header (diagnosis : t) : Fmt.t =
  Fmt.join [ title diagnosis; synopsis diagnosis ]

let get_notes (diagnosis : t) : Info.t list =
  match diagnosis with
  | Error error -> Error.get_notes error
  | Warning warning -> Warning.get_notes warning
  | Info _ -> []

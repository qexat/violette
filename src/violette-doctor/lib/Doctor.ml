open Diagnostic

type t = { diagnostics : Diagnostic.t Dynarray.t }

let create () : t = { diagnostics = Dynarray.create () }

let add_error (error : Error.t) (span : Span.t) (doctor : t)
  : unit
  =
  Dynarray.add_last
    doctor.diagnostics
    { diagnosis = Error error; span }

let add_warning
      (warning : Warning.t)
      (span : Span.t)
      (doctor : t)
  : unit
  =
  Dynarray.add_last
    doctor.diagnostics
    { diagnosis = Warning warning; span }

let add_info (info : Info.t) (span : Span.t) (doctor : t) : unit
  =
  Dynarray.add_last
    doctor.diagnostics
    { diagnosis = Info info; span }

let review ~(mode : Mode.t) (doctor : t) : Review.t =
  let diagnostics = Dynarray.to_list doctor.diagnostics in
  Review.create
    (Review.decide diagnostics)
    (List.map (Diagnostic.render ~mode) diagnostics)

let clear (doctor : t) : unit =
  Dynarray.clear doctor.diagnostics

module Review = Review

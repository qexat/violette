type 'target t =
  { artifact : 'target Artifact.t option
  ; diagnostics : Diagnostic.t list
  }

let create
      (type target)
      ~(artifact : target Artifact.t option)
      ~(diagnostics : Diagnostic.t list)
  : target t
  =
  { artifact; diagnostics }

let bind
      (type a1 a2)
      (output : a1 t)
      (func : a1 Artifact.t -> a2 t)
  : a2 t
  =
  match output.artifact with
  | None ->
    create ~artifact:None ~diagnostics:output.diagnostics
  | Some artifact -> func artifact

open Ext

type t =
  { doctor : Doctor.t
  ; file : File.t
  }

let create (doctor : Doctor.t) (file : File.t) : t =
  { doctor; file }

let to_lambda_core (compiler : t)
  : Target.lambda_core Artifact.t option
  =
  let tokenizer =
    Tokenizer.create compiler.doctor compiler.file
  in
  let*? tokens = Tokenizer.tokenize tokenizer in
  let parser =
    Parser.create compiler.doctor compiler.file tokens
  in
  let*? expr = Parser.parse parser in
  let term = Surface_lowering.lower expr in
  Some (Artifact.Lambda_core term)

let compile
      (type target)
      ~(target : target Target.t)
      (compiler : t)
  : target Output.t
  =
  match target with
  | Lambda_core ->
    Output.create
      ~artifact:(to_lambda_core compiler)
      ~diagnostics:
        (* TODO: we should not do that conversion here. *)
        (Dynarray.to_list compiler.doctor.diagnostics)

module Target = Target
module Artifact = Artifact
module Output = Output

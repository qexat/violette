type 'target t =
  | Lambda_core : Core_term.t -> Target.lambda_core t

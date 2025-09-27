type t =
  (* pairs *)
  | Brace_left (* { *)
  | Brace_right (* } *)
  | Paren_left (* ( *)
  | Paren_right (* ) *)
  (* key operators *)
  | Equal (* = *)
  | Lambda (* \ *)
  | Arrow_right (* -> *)
  (* keywords *)
  | Let
  (* misc *)
  | Identifier_lowercase
  | Binary_numeric_literal
  | Decimal_numeric_literal
  | Hexadecimal_numeric_literal
  | Semicolon

let repr : t -> Better_fmt.t = function
  | Brace_left -> Repr.punctuation "{"
  | Brace_right -> Repr.punctuation "}"
  | Paren_left -> Repr.punctuation "("
  | Paren_right -> Repr.punctuation ")"
  | Equal -> Repr.operator "="
  | Lambda -> Repr.keyword "\\"
  | Arrow_right -> Repr.operator "->"
  | Let -> Repr.keyword "let"
  | Identifier_lowercase -> Repr.opaque "identifier"
  | Binary_numeric_literal
  | Decimal_numeric_literal
  | Hexadecimal_numeric_literal -> Repr.opaque "numeric literal"
  | Semicolon -> Repr.punctuation ";"

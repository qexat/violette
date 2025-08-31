open Ext
open Fmt

type t =
  (* --- lexical analysis --- *)
  | Numeric_literal_cannot_have_a_leading_zero of string
  | Unexpected_end_of_file
  | Unrecognized_token of string
  (* --- syntactic analysis --- *)
  | Expected_expression
  | Expected_parameter_in_lambda
  | Expected_token of Token_type.t
  (* TODO: store span of opening brace *)
  | Unmatched_brace
  (* TODO: store span of opening parenthesis *)
  | Unmatched_parenthesis
  (* --- interpretation --- *)
  (* TODO: store span of the whole application *)
  | Illegal_application of Core_term_normal_form.t
  | Unbound_variable of string

let repr (_ : t) : Fmt.t = Repr.opaque "error type"

let synopsis (error_type : t) : Fmt.t =
  match error_type with
  | Numeric_literal_cannot_have_a_leading_zero _ ->
    Repr.text
      "numeric literals without an explicit base cannot have a \
       leading zero"
  | Unexpected_end_of_file -> Repr.text "unexpected end of file"
  | Unrecognized_token lexeme ->
    Repr.text "unrecognized token"
    ++ raw " "
    ++ Repr.string lexeme
  | Expected_expression -> Repr.text "an expression is expected"
  | Expected_parameter_in_lambda ->
    Repr.text "expected a parameter for the function"
  | Expected_token token_type ->
    Repr.text "expected a token"
    ++ raw " "
    ++ Token_type.repr token_type
  | Unmatched_brace -> Repr.text "found an unmatched brace"
  | Unmatched_parenthesis ->
    Repr.text "found an unmatched parenthesis"
  | Illegal_application normal_form ->
    Repr.text "illegal application of"
    ++ raw " "
    ++ Core_term_normal_form.repr normal_form
  | Unbound_variable name ->
    join
      ~on:(raw " ")
      [ Repr.text "variable"
      ; Repr.identifier name
      ; Repr.text "was not found in this scope"
      ]

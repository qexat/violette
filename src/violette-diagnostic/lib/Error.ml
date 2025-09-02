open Ext

type t =
  (* --- lexical analysis --- *)
  | Identifiers_cannot_start_with_underscore
  | Numeric_literal_cannot_have_leading_zero
  | Unexpected_end_of_file
  | Unrecognized_token of string
  (* --- syntactic analysis --- *)
  | Expected_expression
  | Expected_parameter_in_lambda
  | Expected_token of Token_type.t
  | Unexpected_token of Token.t
  | Unmatched_brace of Span.t
  | Unmatched_parenthesis of Span.t
  (* --- interpretation --- *)
  | Illegal_application of Core_term_normal_form.t
  | Unbound_variable of string

let repr (_ : t) : Fmt.t = Repr.opaque "error"

let synopsis (error : t) : Fmt.t =
  match error with
  | Identifiers_cannot_start_with_underscore ->
    Repr.text "identifiers cannot start with an underscore"
  | Numeric_literal_cannot_have_leading_zero ->
    Repr.text
      "numeric literals without an explicit base cannot have \
       leading zeros"
  | Unexpected_end_of_file -> Repr.text "unexpected end of file"
  | Unrecognized_token lexeme ->
    Fmt.join
      [ Repr.text "unrecognized token"; Repr.string lexeme ]
  | Expected_expression -> Repr.text "an expression is expected"
  | Expected_parameter_in_lambda ->
    Repr.text "expected a parameter for the function"
  | Expected_token token_type ->
    Fmt.join
      [ Repr.text "expected a token of the type"
      ; Token_type.repr token_type
      ]
  | Unexpected_token token ->
    Fmt.join
      [ Repr.text "unexpected token"; Token_type.repr token.ty ]
  | Unmatched_brace _ -> Repr.text "found an unmatched brace"
  | Unmatched_parenthesis _ ->
    Repr.text "found an unmatched parenthesis"
  | Illegal_application func ->
    Fmt.join
      [ Repr.text "illegal application of"
      ; Core_term_normal_form.repr func
      ]
  | Unbound_variable name ->
    Fmt.join
      [ Repr.text "variable"
      ; Repr.identifier name
      ; Repr.text "was not found in this scope"
      ]

let get_notes (error : t) : Info.t list =
  match error with
  | Identifiers_cannot_start_with_underscore ->
    [ Leading_underscores_are_reserved ]
  | _ -> []

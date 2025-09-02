open Ext
open Diagnostic

type t =
  { doctor : Doctor.t
  ; file : File.t
  ; tokens : Token.t list
  ; mutable position : int
  ; brace_stack : Span.t Stack.t
  ; paren_stack : Span.t Stack.t
  }

let create
      (doctor : Doctor.t)
      (file : File.t)
      (tokens : Token.t list)
  : t
  =
  { doctor
  ; file
  ; tokens
  ; position = 0
  ; brace_stack = Stack.create ()
  ; paren_stack = Stack.create ()
  }

let repr
      { doctor = _
      ; file
      ; tokens
      ; position
      ; brace_stack = _
      ; paren_stack = _
      }
  : Fmt.t
  =
  Repr.record
    "Parser"
    [ ("doctor", Repr.opaque "doctor")
    ; ("file", File.repr file)
    ; ("tokens", Repr.list_field (List.map Token.repr tokens))
    ; ("position", Repr.int position)
    ; ("brace_stack", Repr.opaque "stack")
    ; ("paren_stack", Repr.opaque "stack")
    ]

let get_lexeme (token : Token.t) (parser : t) : string =
  match token.ty with
  | Brace_left -> "{"
  | Brace_right -> "}"
  | Paren_left -> "("
  | Paren_right -> ")"
  | Equal -> "="
  | Lambda -> "\\"
  | Arrow_right -> "->"
  | Let -> "let"
  | Identifier_lowercase
  | Binary_numeric_literal
  | Decimal_numeric_literal
  | Hexadecimal_numeric_literal ->
    String.sub parser.file.contents token.position token.length
  | Semicolon -> ";"

let get_span (parser : t) : Span.t =
  Span.from_offset parser.file parser.position parser.position

let peek (parser : t) : Token.t option =
  List.nth_opt parser.tokens parser.position

let peek_type (parser : t) : Token_type.t option =
  Option.map (fun token -> token.Token.ty) (peek parser)

let is_at_end (parser : t) : bool =
  match peek parser with
  | None -> true
  | Some _ -> false

let advance (parser : t) =
  parser.position <- parser.position + 1

let consume (parser : t) : Token.t option =
  let*? token = peek parser in
  advance parser;
  Some token

let add_error (error : Error.t) (parser : t) : unit =
  Doctor.add_error error (get_span parser) parser.doctor

let try_consume (token_type : Token_type.t) (parser : t)
  : Token.t option
  =
  match peek parser with
  | Some token when token.ty = token_type ->
    advance parser;
    Some token
  | _ -> None

let rec try_consume_one_of
          (token_types : Token_type.t list)
          (parser : t)
  : Token.t option
  =
  match token_types with
  | [] -> None
  | first :: rest ->
    (match try_consume first parser with
     | Some token -> Some token
     | None -> try_consume_one_of rest parser)

let expect
      ?(error : Error.t option)
      (token_type : Token_type.t)
      (parser : t)
  : Token.t option
  =
  match try_consume token_type parser with
  | Some token -> Some token
  | None ->
    add_error
      (Option.value error ~default:(Expected_token token_type))
      parser;
    None

let rec expect_one_of
          ~(error : Error.t)
          (types : Token_type.t list)
          (parser : t)
  : Token.t option
  =
  match types with
  | [] ->
    add_error error parser;
    None
  | first :: rest ->
    (match try_consume first parser with
     | Some token -> Some token
     | None -> expect_one_of ~error rest parser)

let matches (ty : Token_type.t) (parser : t) : bool =
  match try_consume ty parser with
  | None -> false
  | Some _ -> true

let eat_maybe (ty : Token_type.t) (parser : t) : unit =
  let _ = matches ty parser in
  ()

let push_brace (parser : t) : unit =
  Stack.push (get_span parser) parser.brace_stack

let pop_brace (parser : t) : Span.t =
  Stack.pop parser.brace_stack

let push_paren (parser : t) : unit =
  Stack.push (get_span parser) parser.paren_stack

let pop_paren (parser : t) : Span.t =
  Stack.pop parser.paren_stack

let rec parse (parser : t) : Surface_term.expr option =
  if is_at_end parser then None else parse_let parser

and parse_let (parser : t) : Surface_term.expr option =
  if not (matches Let parser)
  then parse_function parser
  else
    let*? name = expect Identifier_lowercase parser in
    let*? _ = expect Equal parser in
    let*? body = parse_function parser in
    Some (Surface_term.Let (get_lexeme name parser, body))

and parse_function (parser : t) : Surface_term.expr option =
  if not (matches Lambda parser)
  then parse_apply parser
  else (
    match parse_params parser with
    | [] ->
      add_error Expected_parameter_in_lambda parser;
      None
    | params ->
      let*? _ = expect Arrow_right parser in
      let*? body = parse_apply parser in
      Some (Surface_term.Function (params, body)))

and parse_params (parser : t) : string list =
  match try_consume Identifier_lowercase parser with
  | None -> []
  | Some first -> get_lexeme first parser :: parse_params parser

and parse_apply (parser : t) : Surface_term.expr option =
  let*? expr = parse_block parser in
  match parse_args parser with
  | [] -> Some expr
  | args -> Some (Surface_term.Apply (expr, args))

and parse_args (parser : t) : Surface_term.expr list =
  match parse_block parser with
  | None -> []
  | Some first -> first :: parse_args parser

and parse_block (parser : t) : Surface_term.expr option =
  if not (matches Brace_left parser)
  then parse_atom parser
  else (
    push_brace parser;
    let exprs = parse_several_expressions parser in
    eat_maybe Semicolon parser;
    let*? _ =
      expect
        ~error:(Unmatched_brace (pop_brace parser))
        Brace_right
        parser
    in
    Some (Surface_term.Block exprs))

and parse_several_expressions (parser : t)
  : Surface_term.expr list
  =
  match parse parser with
  | None -> []
  | Some first ->
    if not (matches Semicolon parser)
    then [ first ]
    else first :: parse_several_expressions parser

and parse_atom (parser : t) : Surface_term.expr option =
  parse_unit parser

and parse_unit (parser : t) : Surface_term.expr option =
  if not (matches Paren_left parser)
  then parse_numeric_literal parser
  else (
    push_paren parser;
    if not (matches Paren_right parser)
    then (
      add_error
        (Unmatched_parenthesis (pop_paren parser))
        parser;
      None)
    else Some Unit)

and parse_numeric_literal (parser : t)
  : Surface_term.expr option
  =
  match
    try_consume_one_of
      [ Binary_numeric_literal
      ; Decimal_numeric_literal
      ; Hexadecimal_numeric_literal
      ]
      parser
  with
  | None -> parse_identifier parser
  | Some token ->
    Some
      (Natural
         (Int64.of_int
            (int_of_string (get_lexeme token parser))))

and parse_identifier (parser : t) : Surface_term.expr option =
  match try_consume Identifier_lowercase parser with
  | None ->
    fail_parsing parser;
    None
  | Some token -> Some (Variable (get_lexeme token parser))

and fail_parsing (parser : t) : unit =
  match peek_type parser with
  | None -> add_error Unexpected_end_of_file parser
  | Some Brace_right ->
    add_error (Unmatched_brace (get_span parser)) parser
  | Some Paren_right ->
    add_error (Unmatched_brace (get_span parser)) parser
  | Some (Arrow_right | Equal | Semicolon) ->
    add_error
      (Unexpected_token (Option.get (peek parser)))
      parser
  | Some _ -> add_error Expected_expression parser

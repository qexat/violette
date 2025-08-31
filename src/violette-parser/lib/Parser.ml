open Ext

type t =
  { file : File.t
  ; tokens : Token.t list
  ; mutable position : int
  }

let create (file : File.t) (tokens : Token.t list) : t =
  { file; tokens; position = 0 }

let repr { file; tokens; position } : Fmt.t =
  Repr.record
    "Parser"
    [ ("file", File.repr file)
    ; ("tokens", Repr.list_field (List.map Token.repr tokens))
    ; ("position", Repr.int position)
    ]

let get_lexeme (parser : t) (token : Token.t) : string =
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

let peek (parser : t) : Token.t option =
  List.nth_opt parser.tokens parser.position

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

let make_error (error_type : Error.Type.t) (parser : t)
  : Error.t
  =
  { ty = error_type
  ; span =
      Span.from_offset
        parser.file
        parser.position
        parser.position
  }

let expect
      ?(error_type : Error.Type.t option)
      (parser : t)
      (token_type : Token_type.t)
  : (Token.t, Error.t) result
  =
  match peek parser with
  | Some token when token.ty = token_type ->
    advance parser;
    Ok token
  | _ ->
    Error
      (make_error
         (Option.value
            error_type
            ~default:(Expected_token token_type))
         parser)

let rec expect_one_of (parser : t) (types : Token_type.t list)
  : Token.t option
  =
  match types with
  | [] -> None
  | first :: rest ->
    (match expect parser first with
     | Ok token -> Some token
     | Error _ -> expect_one_of parser rest)

let matches (parser : t) (ty : Token_type.t) : bool =
  match expect parser ty with
  | Error _ -> false
  | Ok _ -> true

let eat_maybe (parser : t) (ty : Token_type.t) : unit =
  let _ = matches parser ty in
  ()

let rec parse (parser : t) : (Surface_term.expr, Error.t) result
  =
  parse_let parser

and parse_let (parser : t) : (Surface_term.expr, Error.t) result
  =
  if not (matches parser Let)
  then parse_function parser
  else
    let*! name = expect parser Identifier_lowercase in
    let*! _ = expect parser Equal in
    let*! body = parse_function parser in
    Ok (Surface_term.Let (get_lexeme parser name, body))

and parse_function (parser : t)
  : (Surface_term.expr, Error.t) result
  =
  if not (matches parser Lambda)
  then parse_apply parser
  else (
    match parse_params parser with
    | [] ->
      Error (make_error Expected_parameter_in_lambda parser)
    | params ->
      let*! _ = expect parser Arrow_right in
      let*! body = parse_apply parser in
      Ok (Surface_term.Function (params, body)))

and parse_params (parser : t) : string list =
  match expect parser Identifier_lowercase with
  | Error _ -> []
  | Ok first -> get_lexeme parser first :: parse_params parser

and parse_apply (parser : t)
  : (Surface_term.expr, Error.t) result
  =
  let*! expr = parse_block parser in
  match parse_args parser with
  | [] -> Ok expr
  | args -> Ok (Surface_term.Apply (expr, args))

and parse_args (parser : t) : Surface_term.expr list =
  match parse_block parser with
  | Error _ -> []
  | Ok first -> first :: parse_args parser

and parse_block (parser : t)
  : (Surface_term.expr, Error.t) result
  =
  if not (matches parser Brace_left)
  then parse_atom parser
  else (
    let exprs = parse_several_expressions parser in
    eat_maybe parser Semicolon;
    let*! _ =
      expect ~error_type:Unmatched_brace parser Brace_right
    in
    Ok (Surface_term.Block exprs))

and parse_several_expressions (parser : t)
  : Surface_term.expr list
  =
  match parse parser with
  | Error _ -> []
  | Ok first ->
    if not (matches parser Semicolon)
    then [ first ]
    else first :: parse_several_expressions parser

and parse_atom (parser : t)
  : (Surface_term.expr, Error.t) result
  =
  if matches parser Paren_left
  then
    if matches parser Paren_right
    then Ok Unit
    else Error (make_error Unmatched_parenthesis parser)
  else (
    match
      expect_one_of
        parser
        [ Binary_numeric_literal
        ; Decimal_numeric_literal
        ; Hexadecimal_numeric_literal
        ]
    with
    | Some token ->
      Ok
        (Natural
           (Int64.of_int
              (int_of_string (get_lexeme parser token))))
    | None ->
      (match expect parser Identifier_lowercase with
       | Error _ ->
         Error (make_error Expected_expression parser)
       | Ok token -> Ok (Variable (get_lexeme parser token))))

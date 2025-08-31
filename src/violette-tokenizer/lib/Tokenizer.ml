open Ansifmt
open Ext

type t =
  { file : File.t
  ; mutable start : int
  ; mutable current : int
  ; tokens : Token.t Dynarray.t
  }

let create (file : File.t) : t =
  { file; start = 0; current = 0; tokens = Dynarray.create () }

let repr { file; start; current; tokens } : Fmt.t =
  Repr.record
    "Tokenizer"
    [ ("file", File.repr file)
    ; ("start", Repr.int start)
    ; ("current", Repr.int current)
    ; ( "tokens"
      , Repr.list_field
          (tokens |> Dynarray.to_list |> List.map Token.repr) )
    ]

let get_lexeme (tokenizer : t) : string =
  String.sub
    tokenizer.file.contents
    tokenizer.start
    (tokenizer.current - tokenizer.start)

let is_at_end (tokenizer : t) : bool =
  tokenizer.current >= String.length tokenizer.file.contents

let to_next (tokenizer : t) =
  tokenizer.start <- tokenizer.current

let peek (tokenizer : t) : string option =
  if is_at_end tokenizer
  then None
  else
    Some
      (String.sub tokenizer.file.contents tokenizer.current 1)

let advance (tokenizer : t) =
  tokenizer.current <- tokenizer.current + 1

let consume (tokenizer : t) : string option =
  let*? token = peek tokenizer in
  advance tokenizer;
  Some token

let matches (tokenizer : t) (grapheme : string) : bool =
  match peek tokenizer with
  | Some poke when grapheme = poke -> true
  | _ -> false

let rec consume_while
          (tokenizer : t)
          (predicate : string -> bool)
  =
  match peek tokenizer with
  | Some grapheme when predicate grapheme ->
    advance tokenizer;
    consume_while tokenizer predicate
  | _ -> ()

(* errors *)

let make_error (tokenizer : t) (error_type : Error.Type.t)
  : Error.t
  =
  { ty = error_type
  ; span =
      Span.from_offset
        tokenizer.file
        tokenizer.start
        tokenizer.current
  }

(* specialized functions *)

let tokenize_identifier_lowercase_or_keyword (tokenizer : t)
  : (Token_type.t, Error.t) result
  =
  consume_while tokenizer Predicates.is_identifier;
  match get_lexeme tokenizer with
  | "let" -> Ok Let
  | _ -> Ok Identifier_lowercase

let tokenize_plain_numeric_literal (tokenizer : t)
  : (Token_type.t, Error.t) result
  =
  consume_while tokenizer Predicates.is_decimal_digit;
  Ok Decimal_numeric_literal

let tokenize_binary_literal (tokenizer : t)
  : (Token_type.t, Error.t) result
  =
  consume_while tokenizer Predicates.is_binary_digit;
  Ok Binary_numeric_literal

let tokenize_hexadecimal_literal (tokenizer : t)
  : (Token_type.t, Error.t) result
  =
  consume_while tokenizer Predicates.is_hexadecimal_digit;
  Ok Hexadecimal_numeric_literal

let rec scan (tokenizer : t) : (Token_type.t, Error.t) result =
  match consume tokenizer with
  | None -> Error (make_error tokenizer Unexpected_end_of_file)
  | Some ("\n" | "\r" | "\t" | " ") ->
    to_next tokenizer;
    scan tokenizer
  | Some "{" -> Ok Brace_left
  | Some "}" -> Ok Brace_right
  | Some "(" -> Ok Paren_left
  | Some ")" -> Ok Paren_right
  | Some "=" -> Ok Equal
  | Some "\\" -> Ok Lambda
  | Some "-" ->
    (match peek tokenizer with
     | Some ">" ->
       advance tokenizer;
       Ok Arrow_right
     | _ ->
       Error (make_error tokenizer (Unrecognized_token "-")))
  | Some ";" -> Ok Semicolon
  | Some "0" ->
    (match peek tokenizer with
     | Some ("b" | "B") ->
       advance tokenizer;
       tokenize_binary_literal tokenizer
     | Some ("x" | "X") ->
       advance tokenizer;
       tokenize_hexadecimal_literal tokenizer
     | Some grapheme when Predicates.is_decimal_digit grapheme
       ->
       Error
         (make_error
            tokenizer
            (Numeric_literal_cannot_have_a_leading_zero grapheme))
     | _ -> Ok Decimal_numeric_literal)
  | Some grapheme when Predicates.is_decimal_digit grapheme ->
    tokenize_plain_numeric_literal tokenizer
  | Some grapheme when Predicates.starts_identifier grapheme ->
    tokenize_identifier_lowercase_or_keyword tokenizer
  | Some grapheme ->
    Error (make_error tokenizer (Unrecognized_token grapheme))

let rec tokenize (tokenizer : t)
  : (Token.t list, Error.t) result
  =
  if is_at_end tokenizer
  then Ok (Dynarray.to_list tokenizer.tokens)
  else (
    to_next tokenizer;
    let*! token_type = scan tokenizer in
    Dynarray.add_last
      tokenizer.tokens
      { ty = token_type
      ; position = tokenizer.start
      ; length = tokenizer.current - tokenizer.start
      };
    tokenize tokenizer)

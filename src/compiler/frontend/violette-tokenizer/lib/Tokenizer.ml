open Ext
open Diagnostic

type t =
  { doctor : Doctor.t
  ; file : File.t
  ; mutable start : int
  ; mutable current : int
  ; (* this buffer exists to support unicode characters *)
    buffer : Buffer.t
  ; tokens : Token.t Dynarray.t
  }

let create (doctor : Doctor.t) (file : File.t) : t =
  { doctor
  ; file
  ; start = 0
  ; current = 0
  ; buffer = Buffer.create 4
  ; tokens = Dynarray.create ()
  }

let repr
      { doctor = _; file; start; current; buffer = _; tokens }
  : Fmt.t
  =
  Repr.record
    "Tokenizer"
    [ ("doctor", Repr.opaque "doctor")
    ; ("file", File.repr file)
    ; ("start", Repr.int start)
    ; ("current", Repr.int current)
    ; ("buffer", Repr.opaque "buffer")
    ; ( "tokens"
      , Repr.list_field
          (tokens |> Dynarray.to_list |> List.map Token.repr) )
    ]

let get_lexeme (offset : int) (length : int) (tokenizer : t)
  : string
  =
  String.sub tokenizer.file.contents offset length

let get_lexeme_current (tokenizer : t) : string =
  get_lexeme
    tokenizer.start
    (tokenizer.current - tokenizer.start)
    tokenizer

let is_at_end (tokenizer : t) : bool =
  tokenizer.current >= String.length tokenizer.file.contents

let to_next (tokenizer : t) =
  tokenizer.start <- tokenizer.current

let peek (tokenizer : t) : string option =
  if is_at_end tokenizer
  then None
  else (
    Buffer.clear tokenizer.buffer;
    Buffer.add_utf_8_uchar
      tokenizer.buffer
      (Uchar.utf_decode_uchar
         (String.get_utf_8_uchar
            tokenizer.file.contents
            tokenizer.current));
    Some (Buffer.contents tokenizer.buffer))

let advance ?(steps = 1) (tokenizer : t) =
  tokenizer.current <- tokenizer.current + steps

let consume (tokenizer : t) : string option =
  let*? grapheme = peek tokenizer in
  advance ~steps:(String.length grapheme) tokenizer;
  Some grapheme

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

let error (error : Error.t) (tokenizer : t) : unit =
  Doctor.add_error
    error
    (Span.from_offset
       tokenizer.file
       tokenizer.start
       tokenizer.current)
    tokenizer.doctor

(* specialized functions *)

let tokenize_identifier_lowercase_or_keyword (tokenizer : t)
  : Token_type.t option
  =
  consume_while tokenizer Predicates.is_identifier;
  match get_lexeme_current tokenizer with
  | "let" -> Some Let
  | _ -> Some Identifier_lowercase

let tokenize_plain_numeric_literal (tokenizer : t)
  : Token_type.t option
  =
  consume_while tokenizer Predicates.is_decimal_digit;
  Some Decimal_numeric_literal

let tokenize_binary_literal (tokenizer : t)
  : Token_type.t option
  =
  consume_while tokenizer Predicates.is_binary_digit;
  Some Binary_numeric_literal

let tokenize_hexadecimal_literal (tokenizer : t)
  : Token_type.t option
  =
  consume_while tokenizer Predicates.is_hexadecimal_digit;
  Some Hexadecimal_numeric_literal

let rec scan (tokenizer : t) : Token_type.t option =
  match consume tokenizer with
  | None ->
    error Unexpected_end_of_file tokenizer;
    None
  | Some ("\n" | "\r" | "\t" | " ") ->
    to_next tokenizer;
    scan tokenizer
  | Some "{" -> Some Brace_left
  | Some "}" -> Some Brace_right
  | Some "(" -> Some Paren_left
  | Some ")" -> Some Paren_right
  | Some "=" -> Some Equal
  | Some "\\" -> Some Lambda
  | Some "-" ->
    (match peek tokenizer with
     | Some ">" ->
       advance tokenizer;
       Some Arrow_right
     | _ ->
       error (Unrecognized_token "-") tokenizer;
       None)
  | Some ";" -> Some Semicolon
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
       error Numeric_literal_cannot_have_leading_zero tokenizer;
       None
     | _ -> Some Decimal_numeric_literal)
  | Some grapheme when Predicates.is_decimal_digit grapheme ->
    tokenize_plain_numeric_literal tokenizer
  | Some "_" ->
    (match peek tokenizer with
     | Some grapheme when Predicates.is_identifier grapheme ->
       error Identifiers_cannot_start_with_underscore tokenizer;
       None
     | _ -> Some Identifier_lowercase)
  | Some grapheme when Predicates.starts_identifier grapheme ->
    tokenize_identifier_lowercase_or_keyword tokenizer
  | Some grapheme ->
    error (Unrecognized_token grapheme) tokenizer;
    None

let check_token (token : Token.t) (tokenizer : t) : unit =
  (* lexeme is lazy because we only need it in edge cases but
     the code looks super ugly if we fetch it in every branch *)
  let lexeme =
    lazy (get_lexeme token.position token.length tokenizer)
  in
  match token.ty with
  | Decimal_numeric_literal
    when String.starts_with ~prefix:"0" (Lazy.force lexeme) ->
    error Numeric_literal_cannot_have_leading_zero tokenizer
  | Identifier_lowercase
    when String.starts_with ~prefix:"_" (Lazy.force lexeme) ->
    error Identifiers_cannot_start_with_underscore tokenizer
  | _ -> ()

let check (tokens : Token.t list) (tokenizer : t) : unit =
  List.iter (Fun.flip check_token tokenizer) tokens

let rec tokenize_lenient (tokenizer : t) : Token.t list option =
  if is_at_end tokenizer
  then Some (Dynarray.to_list tokenizer.tokens)
  else (
    to_next tokenizer;
    let*? token_type = scan tokenizer in
    Dynarray.add_last
      tokenizer.tokens
      { ty = token_type
      ; position = tokenizer.start
      ; length = tokenizer.current - tokenizer.start
      };
    tokenize_lenient tokenizer)

let tokenize (tokenizer : t) : Token.t list option =
  let*? tokens = tokenize_lenient tokenizer in
  check tokens tokenizer;
  Some tokens

open Ansifmt
open Ansi
open Ext
open Fmt

let character_quote : Fmt.t = stylize (raw "'") `Dim

let character (contents : string) : Fmt.t =
  stylize
    (concat
       [ character_quote
       ; raw (String.escaped contents)
       ; character_quote
       ])
    (`Foreground Color.cyan)

let function_name (name : string) : Fmt.t =
  stylize (raw name) (`Foreground Color.blue)

let identifier (name : string) : Fmt.t = raw name
let indent : Fmt.t = raw "  "

let keyword (name : string) : Fmt.t =
  stylize (raw name) (`Foreground Color.magenta & `Bold)

let numeric (lexeme : string) : Fmt.t =
  stylize (raw lexeme) (`Foreground Color.cyan & `Italic)

let opaque (name : string) : Fmt.t =
  stylize
    (concat [ raw "<"; raw name; raw ">" ])
    (`Foreground Color.magenta)

let operator (lexeme : string) : Fmt.t =
  stylize (raw lexeme) (`Foreground Color.magenta)

let parameter (name : string) : Fmt.t =
  stylize (identifier name) `Italic

let punctuation (lexeme : string) : Fmt.t =
  stylize (raw lexeme) `Dim

let string_quote : Fmt.t = stylize (raw "\"") `Dim

let string (contents : string) : Fmt.t =
  stylize
    (concat
       [ string_quote
       ; raw (String.escaped contents)
       ; string_quote
       ])
    (`Foreground Color.green)

let text (contents : string) : Fmt.t = raw contents

let type_name (name : string) : Fmt.t =
  stylize (raw name) (`Foreground Color.yellow)

let application (func : Fmt.t) (args : Fmt.t list) : Fmt.t =
  join ~on:(raw " ") (func :: args)

let block (expressions : Fmt.t list) : Fmt.t =
  match expressions with
  | [] -> join [ punctuation "{"; punctuation "}" ]
  | first :: rest ->
    concat
      [ raw "\n"
      ; join
          ~on:(concat [ raw "\n"; indent; punctuation ";" ])
          ((punctuation "{" ++ first) :: rest)
      ; raw "\n"
      ; indent
      ; punctuation "}"
      ]

let lambda (params : string list) (body : Fmt.t) : Fmt.t =
  keyword "\\"
  ++ join
       [ join (List.map parameter params); operator "->"; body ]

let let_definition
      (name : string)
      ?(params : string list = [])
      (body : Fmt.t)
  : Fmt.t
  =
  join
    [ keyword "let"
    ; join (identifier name :: List.map parameter params)
    ; operator "="
    ; body
    ]

(* OCaml specific *)

let int (value : int) : Fmt.t = numeric (Int.to_string value)

let tuple (values : Fmt.t list) : Fmt.t =
  concat
    [ punctuation "("
    ; join ~on:(concat [ punctuation ","; raw " " ]) values
    ; punctuation ")"
    ]

let field_name (name : string) : Fmt.t =
  stylize (raw name) (`Foreground Color.blue)

let field (name : string) (value : Fmt.t) : Fmt.t =
  join [ field_name name; operator "="; value ]

let field_list (fields : (string * Fmt.t) list) : Fmt.t =
  fields
  |> List.map (fun (name, value) -> field name value)
  |> join ~on:(concat [ raw " "; punctuation ";"; raw " " ])

let list_field (values : Fmt.t list) : Fmt.t =
  match values with
  | [] -> punctuation "[]"
  | _ ->
    join
      ~on:(raw " ")
      [ punctuation "["
      ; join
          ~on:(concat [ raw " "; punctuation ";"; raw " " ])
          values
      ; punctuation "]"
      ]

let record (name : string) (fields : (string * Fmt.t) list)
  : Fmt.t
  =
  [ type_name name
  ; punctuation "{"
  ; field_list fields
  ; punctuation "}"
  ]
  |> join ~on:(raw " ")

(* diagnostic stuff *)

let error (contents : string) : Fmt.t =
  stylize (raw contents) (`Foreground Color.red & `Bold)

let warning (contents : string) : Fmt.t =
  stylize (raw contents) (`Foreground Color.yellow & `Bold)

let info (contents : string) : Fmt.t =
  stylize (raw contents) (`Foreground Color.blue & `Bold)

(* bytecode *)

let instruction (mnemonic : string) (args : Fmt.t list) : Fmt.t =
  let mnemonic_fmt = keyword mnemonic in
  match args with
  | [] -> mnemonic_fmt
  | last :: [] -> join ~on:(raw "\t") [ mnemonic_fmt; last ]
  | _ -> join ~on:(raw "\t") [ mnemonic_fmt; tuple args ]

let section (name : string) (instructions : Fmt.t list) : Fmt.t =
  join
    ~on:(raw "\n" ++ indent)
    (function_name name :: instructions)

let bytecode_directive ?(args : Fmt.t list = []) (name : string)
  : Fmt.t
  =
  join (keyword ("@" ^ name) :: args)

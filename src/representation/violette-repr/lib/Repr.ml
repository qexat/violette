open Ansifmt
open Ansi
open Better_fmt

let character_quote : Better_fmt.t = stylize (raw "'") `Dim

let character (contents : string) : Better_fmt.t =
  stylize
    (concat
       [ character_quote
       ; raw (String.escaped contents)
       ; character_quote
       ])
    (`Foreground Color.cyan)

let function_name (name : string) : Better_fmt.t =
  stylize (raw name) (`Foreground Color.blue)

let identifier (name : string) : Better_fmt.t = raw name
let indent : Better_fmt.t = raw "  "

let keyword (name : string) : Better_fmt.t =
  stylize (raw name) (`Foreground Color.magenta & `Bold)

let numeric (lexeme : string) : Better_fmt.t =
  stylize (raw lexeme) (`Foreground Color.cyan & `Italic)

let opaque (name : string) : Better_fmt.t =
  stylize
    (concat [ raw "<"; raw name; raw ">" ])
    (`Foreground Color.magenta)

let operator (lexeme : string) : Better_fmt.t =
  stylize (raw lexeme) (`Foreground Color.magenta)

let parameter (name : string) : Better_fmt.t =
  stylize (identifier name) `Italic

let punctuation (lexeme : string) : Better_fmt.t =
  stylize (raw lexeme) `Dim

let string_quote : Better_fmt.t = stylize (raw "\"") `Dim

let string (contents : string) : Better_fmt.t =
  stylize
    (concat
       [ string_quote
       ; raw (String.escaped contents)
       ; string_quote
       ])
    (`Foreground Color.green)

let text (contents : string) : Better_fmt.t = raw contents

let type_name (name : string) : Better_fmt.t =
  stylize (raw name) (`Foreground Color.yellow)

let application (func : Better_fmt.t) (args : Better_fmt.t list)
  : Better_fmt.t
  =
  join ~on:(raw " ") (func :: args)

let block (expressions : Better_fmt.t list) : Better_fmt.t =
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

let lambda (params : string list) (body : Better_fmt.t)
  : Better_fmt.t
  =
  keyword "\\"
  ++ join
       [ join (List.map parameter params); operator "->"; body ]

let let_definition
      (name : string)
      ?(params : string list = [])
      (body : Better_fmt.t)
  : Better_fmt.t
  =
  join
    [ keyword "let"
    ; join (identifier name :: List.map parameter params)
    ; operator "="
    ; body
    ]

(* OCaml specific *)

let int (value : int) : Better_fmt.t =
  numeric (Int.to_string value)

let tuple (values : Better_fmt.t list) : Better_fmt.t =
  concat
    [ punctuation "("
    ; join ~on:(concat [ punctuation ","; raw " " ]) values
    ; punctuation ")"
    ]

let field_name (name : string) : Better_fmt.t =
  stylize (raw name) (`Foreground Color.blue)

let field (name : string) (value : Better_fmt.t) : Better_fmt.t =
  join [ field_name name; operator "="; value ]

let field_list (fields : (string * Better_fmt.t) list)
  : Better_fmt.t
  =
  fields
  |> List.map (fun (name, value) -> field name value)
  |> join ~on:(concat [ raw " "; punctuation ";"; raw " " ])

let list_field (values : Better_fmt.t list) : Better_fmt.t =
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

let record
      (name : string)
      (fields : (string * Better_fmt.t) list)
  : Better_fmt.t
  =
  [ type_name name
  ; punctuation "{"
  ; field_list fields
  ; punctuation "}"
  ]
  |> join ~on:(raw " ")

(* diagnostic stuff *)

let error (contents : string) : Better_fmt.t =
  stylize (raw contents) (`Foreground Color.red & `Bold)

let warning (contents : string) : Better_fmt.t =
  stylize (raw contents) (`Foreground Color.yellow & `Bold)

let info (contents : string) : Better_fmt.t =
  stylize (raw contents) (`Foreground Color.blue & `Bold)

(* bytecode *)

let instruction (mnemonic : string) (args : Better_fmt.t list)
  : Better_fmt.t
  =
  let mnemonic_fmt = keyword mnemonic in
  match args with
  | [] -> mnemonic_fmt
  | last :: [] -> join ~on:(raw "\t") [ mnemonic_fmt; last ]
  | _ -> join ~on:(raw "\t") [ mnemonic_fmt; tuple args ]

let section (name : string) (instructions : Better_fmt.t list)
  : Better_fmt.t
  =
  join
    ~on:(raw "\n" ++ indent)
    (function_name name :: instructions)

let bytecode_directive
      ?(args : Better_fmt.t list = [])
      (name : string)
  : Better_fmt.t
  =
  join (keyword ("@" ^ name) :: args)

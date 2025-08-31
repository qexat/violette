open Ansifmt
open Ansi
open Fmt

let character_quote : Fmt.t = stylize (raw "'") `Dim

let character (contents : string) : Fmt.t =
  stylize
    (character_quote
     ++ raw (String.escaped contents)
     ++ character_quote)
    (`Foreground Color.cyan)

let identifier (name : string) : Fmt.t = raw name
let indent : Fmt.t = raw "  "

let keyword (name : string) : Fmt.t =
  stylize (raw name) (`Foreground Color.magenta & `Bold)

let numeric (lexeme : string) : Fmt.t =
  stylize (raw lexeme) (`Foreground Color.cyan & `Italic)

let opaque (name : string) : Fmt.t =
  stylize
    (raw "<" ++ raw name ++ raw ">")
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
    (string_quote
     ++ raw (String.escaped contents)
     ++ string_quote)
    (`Foreground Color.green)

let text (contents : string) : Fmt.t = raw contents

let type_name (name : string) : Fmt.t =
  stylize (raw name) (`Foreground Color.yellow)

let application (func : Fmt.t) (args : Fmt.t list) : Fmt.t =
  join ~on:(raw " ") (func :: args)

let block (expressions : Fmt.t list) : Fmt.t =
  match expressions with
  | [] -> punctuation "{" ++ raw " " ++ punctuation "}"
  | first :: rest ->
    raw "\n"
    ++ (((punctuation "{" ++ first) :: rest
         |> join ~on:(raw "\n" ++ indent ++ punctuation ";"))
        ++ raw "\n"
        ++ indent
        ++ punctuation "}")

let func (params : string list) (body : Fmt.t) : Fmt.t =
  keyword "\\"
  ++ ([ join ~on:(raw " ") (List.map parameter params)
      ; operator "->"
      ; body
      ]
      |> join ~on:(raw " "))

let let_definition (name : string) (body : Fmt.t) : Fmt.t =
  [ keyword "let"; identifier name; operator "="; body ]
  |> join ~on:(raw " ")

(* OCaml specific *)

let int (value : int) : Fmt.t = numeric (Int.to_string value)

let field_name (name : string) : Fmt.t =
  stylize (raw name) (`Foreground Color.blue)

let field (name : string) (value : Fmt.t) : Fmt.t =
  [ field_name name; operator "="; value ] |> join ~on:(raw " ")

let field_list (fields : (string * Fmt.t) list) : Fmt.t =
  fields
  |> List.map (fun (name, value) -> field name value)
  |> join ~on:(raw " " ++ punctuation ";" ++ raw " ")

let list_field (values : Fmt.t list) : Fmt.t =
  match values with
  | [] -> punctuation "[]"
  | _ ->
    join
      ~on:(raw " ")
      [ punctuation "["
      ; join ~on:(raw " " ++ punctuation ";" ++ raw " ") values
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

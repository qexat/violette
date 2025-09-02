let starts_identifier (grapheme : string) : bool =
  "a" <= grapheme && grapheme <= "z"

let is_identifier (grapheme : string) : bool =
  starts_identifier grapheme
  || grapheme = "_"
  || ("0" <= grapheme && grapheme <= "9")

let is_binary_digit (grapheme : string) : bool =
  grapheme = "0" || grapheme = "1"

let is_decimal_digit (grapheme : string) : bool =
  "0" <= grapheme && grapheme <= "9"

let is_hexadecimal_digit (grapheme : string) : bool =
  is_decimal_digit grapheme
  || ("A" <= grapheme && grapheme <= "F")
  || ("a" <= grapheme && grapheme <= "f")

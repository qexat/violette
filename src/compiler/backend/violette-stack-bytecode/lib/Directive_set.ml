open Ext

type t = { entry_point : string }

let repr { entry_point } : Fmt.t =
  Fmt.join_lines
    [ Repr.bytecode_directive
        "start"
        ~args:[ Repr.function_name entry_point ]
    ]

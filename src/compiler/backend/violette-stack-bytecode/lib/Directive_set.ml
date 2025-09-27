type t = { entry_point : string }

let repr { entry_point } : Better_fmt.t =
  Better_fmt.join_lines
    [ Repr.bytecode_directive
        "start"
        ~args:[ Repr.function_name entry_point ]
    ]

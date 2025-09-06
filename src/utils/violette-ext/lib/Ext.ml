let ( let*? ) = Option.bind
let ( let*! ) = Result.bind

let make_index_generator () =
  let state = ref 0 in
  fun () ->
    let value = !state in
    incr state;
    value

let make_dummy_identifier_generator ~(prefix : string) =
  let make_index = make_index_generator () in
  fun () -> Printf.sprintf "$%s:%d" prefix (make_index ())

module Env = Env
module Fmt = Fmt

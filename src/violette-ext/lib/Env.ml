type 'value t = (string * 'value) list

let fetch ~(error : 'error) (name : string) (env : 'value t)
  : ('value, 'error) result
  =
  match List.assoc_opt name env with
  | None -> Error error
  | Some value -> Ok value

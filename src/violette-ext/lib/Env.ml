type 'value t = (string * 'value) list

let fetch (name : string) (env : 'value t) : 'value option =
  List.assoc_opt name env

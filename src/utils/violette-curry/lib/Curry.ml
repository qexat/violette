let curry_left
      (fixpoint : 'fixpoint)
      (items : 'item list)
      ~(fixpoint_map : 'fixpoint -> 'curried)
      ~(item_map : 'item -> 'pivot)
      ~(constructor : 'curried -> 'pivot -> 'curried)
  : 'curried
  =
  let rec tail acc items ~lower_item ~constructor =
    match items with
    | [] -> acc
    | first :: rest ->
      tail
        (constructor acc (lower_item first))
        rest
        ~lower_item
        ~constructor
  in
  tail
    (fixpoint_map fixpoint)
    items
    ~lower_item:item_map
    ~constructor

let curry_right
      (items : 'item list)
      (fixpoint : 'fixpoint)
      ~(fixpoint_map : 'fixpoint -> 'curried)
      ~(item_map : 'item -> 'pivot)
      ~(constructor : 'pivot -> 'curried -> 'curried)
  : 'curried
  =
  let rec tail items acc ~curry_item ~constructor =
    match items with
    | [] -> acc
    | last :: rest ->
      tail
        rest
        (constructor (curry_item last) acc)
        ~curry_item
        ~constructor
  in
  tail
    (List.rev items)
    (fixpoint_map fixpoint)
    ~curry_item:item_map
    ~constructor

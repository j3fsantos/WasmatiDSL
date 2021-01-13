type t = 
    | Not 

let str (op : t) : string = 
  match op with 
  | Not -> "not"

let eval (op : t) (v : Literal.t) : Literal.t = 
  match op, v with 
  | Not, Bool b -> Bool (not b) 
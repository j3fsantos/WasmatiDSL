type t = 
    | Not 

let str (op : t) : string = 
    match op with 
    | Not -> "not"
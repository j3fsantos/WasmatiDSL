type t = 
    | Equal 
    | In 
    | And 

let str (op : t) : string = 
    match op with 
    | Equal -> "="
    | In    -> "in"
    | And   -> "and"
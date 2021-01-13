type t = 
    | Equal 
    | In 
    | And 

let str (op : t) : string = 
    match op with 
    | Equal -> "="
    | In    -> "in"
    | And   -> "and"

let eval (op : t) (v1 : Literal.t) (v2 : Literal.t) : Literal.t = 
  match op with 
  | Equal -> 
      Printf.printf "Called Equal with: %s and %s\n" (Literal.str v1) (Literal.str v2);
      Bool (v1 = v2)
  | In -> 
    (match v1 with 
    | List vs -> Bool (List.mem v2 vs)
    | _ -> raise (Failure "left argument for IN operator is not a list"))
  | And -> 
    (match v1, v2 with 
    | Bool b1, Bool b2 -> 
        Printf.printf "Called And with: %b and %b\n" b1 b2;
        Bool (b1 && b2)
    | _ -> raise (Failure "And with non-boolean arguments"))
type t = 
    | List  of t list 
    | Int   of int  
    | Str   of string 
    | Flt   of float 

let rec to_json (l : t) : string = 
  let fs ls = String.concat ", " (List.map to_json ls) in 
  match l with 
    | Int i  -> Printf.sprintf "{ \"type\": \"INTVAL\", \"value\": %d}" i 
    | Flt f  -> Printf.sprintf "{ \"type\": \"FLTVAL\", \"value\": %f}" f
    | Str s  -> Printf.sprintf "{ \"type\": \"STRVAL\", \"value\": \"%s\"}" s
    | List l -> Printf.sprintf "{ \"type\": \"LSTVAL\", \"value\": [%s]}" (fs l)

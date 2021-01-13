type t = 
    | List  of t list 
    | Int   of int  
    | Str   of string 
    | Flt   of float 
    | Bool  of bool 

let rec to_json (l : t) : string = 
  let fs ls = String.concat ", " (List.map to_json ls) in 
  match l with 
    | Int i  -> Printf.sprintf "{ \"type\": \"INTVAL\", \"value\": %d}" i 
    | Flt f  -> Printf.sprintf "{ \"type\": \"FLTVAL\", \"value\": %f}" f
    | Str s  -> Printf.sprintf "{ \"type\": \"STRVAL\", \"value\": \"%s\"}" s
    | List l -> Printf.sprintf "{ \"type\": \"LSTVAL\", \"value\": [%s]}" (fs l)
    | Bool b -> Printf.sprintf "{ \"type\": \"BOOLVAL\", \"value\": %b}" b

let rec str (l : t) : string = 
  let fs ls = String.concat ", " (List.map str ls) in 
  match l with 
    | Int i  -> string_of_int i
    | Flt f  -> string_of_float f 
    | Str s  -> s
    | List l -> Printf.sprintf "[%s]" (fs l)
    | Bool b -> string_of_bool b 

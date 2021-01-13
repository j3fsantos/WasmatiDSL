type type_t = 
  | I32
  | I64
  | F32
  | F64

type val_t = 
  | Int of int 
  | Flt of float  

let parse_t (stype : string) : type_t = 
  if (stype = "i32") then I32
  else if (stype = "i64") then I64
  else if (stype = "f32") then F32
  else if (stype = "f64") then F64
  else raise (Failure "wrong type")

let parse_vt (stype : string) (sint : string) (sfloat : string) : type_t * val_t = 
  let t = parse_t stype in 
  match t with 
    | I32 | I64 -> (t, Int (int_of_string sint))
    | F32 | F64 -> (t, Flt (float_of_string sfloat))
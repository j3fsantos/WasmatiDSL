type t = 
    | RangeExpr of t * string * t * t 
    | LookUp    of t * string 
    | BinOp     of BinOp.t * t * t
    | UnOp      of UnOp.t * t 
    | EList     of t list 
    | Var       of string 
    | Lit       of Literal.t 
    | Call      of string * t list 

let rec to_json (e : t) : string = 
  let fs es = String.concat ", " (List.map to_json es) in 
  match e with 
    | RangeExpr (e, x, d, p) -> 
        Printf.sprintf "{ \"type\": \"RANGE_EXPR\", \"expr\": %s, \"var\": \"%s\", \"dom\": %s, \"pred\": %s}" 
            (to_json e) x (to_json d) (to_json p)
    | LookUp (e, x) -> 
        Printf.sprintf "{ \"type\": \"LOOKUP_EXPR\", \"base\": %s, \"field\": \"%s\"}" 
            (to_json e) x 
    | BinOp (op, e1, e2) -> 
        Printf.sprintf "{ \"type\": \"BINOP_EXPR\", \"op\": \"%s\", \"left\": %s, \"right\": %s}" 
            (BinOp.str op) (to_json e1) (to_json e2) 
    | UnOp (op, e)  -> 
        Printf.sprintf "{ \"type\": \"UNOP_EXPR\", \"op\": \"%s\", \"arg\": \"%s\"}" 
            (UnOp.str op) (to_json e)
    | EList es -> 
        Printf.sprintf "{ \"type\": \"EXPR_LIST\", \"elems\": [ %s ]}" (fs es)
    | Var x -> 
        Printf.sprintf "{ \"type\": \"VAR_EXPR\", \"name\": \"%s\"}" x 
    | Lit l -> 
        Printf.sprintf "{ \"type\": \"LIT_EXPR\", \"lit\": %s}" (Literal.to_json l)
    | Call (x, es) ->
        Printf.sprintf "{ \"type\": \"CALL_EXPR\", \"var\": \"%s\", \"args\": [%s]}" x (fs es) 


let rec map (f : t -> t) (e : t) : t = 
  let f' = map f in 
  match f e with 
  | RangeExpr (e_r, x, e_d, e_p) ->  RangeExpr (f' e_r, x, f' e_d, f' e_p)
  | LookUp (e, x) -> LookUp (f' e, x)
  | BinOp (op, e1, e2) -> BinOp (op, f' e1, f' e2)
  | UnOp (op, e) -> UnOp (op, f' e)
  | EList es -> EList (List.map f' es)
  | Var x -> Var x 
  | Lit l -> Lit l 
  | Call (f, es) -> Call (f, List.map f' es)

let subst (sbst : Subst.t) (e : t) : t = 
  let f e = 
    match e with 
    | Var x -> 
        let vo = Subst.get sbst x in 
        (match vo with 
        | Some v -> Lit v 
        | None -> Var x)
    | _ ->  e in 
  map f e 
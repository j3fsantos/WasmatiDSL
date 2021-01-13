type t = 
  | AST of int * int 
  | CG  of int * int 
  | CFG of int * int * string   
  | PDGGlobal of int * int * string 
  | PDGLocal of int * int * string 
  | PDGConst of int * int * string * Wasm.type_t * Wasm.val_t
  | PDGFun of int * int * string 


let int_idx (elems : string list) (idx : int) : int =
  int_of_string (List.nth elems idx) 

let parse (str : string) : t = 
  let elems = String.split_on_char ',' str in 
  let source = int_idx elems 0 in 
  let dest = int_idx elems 1 in 
  let edge_type = int_idx elems 2 in
  let label = List.nth elems 3 in
  if (edge_type = 0) then AST (source, dest)
  else if (edge_type = 1) then ( CFG (source, dest, label) )
  else if (edge_type = 3) then ( CG (source, dest) )
  else if (edge_type = 2) then (
    let pdg_type = int_idx elems 4 in 
    if (pdg_type = 0) then (
      let (wasm_t, wasm_v) = Wasm.parse_vt (List.nth elems 5) (List.nth elems 6) (List.nth elems 4) in 
      PDGConst (source, dest, label, wasm_t, wasm_v)
    ) else if (pdg_type = 2) then PDGFun (source, dest, label)
      else if (pdg_type = 3) then PDGGlobal (source, dest, label)
      else if (pdg_type = 4) then PDGLocal (source, dest, label)  
      else (raise (Failure "illegal edge type"))
  ) else (raise (Failure "illegal edge type"))
    
let source (edge : t) : int = 
  match edge with 
  | AST (x, _)
  | CG (x, _)
  | CFG (x, _, _)
  | PDGGlobal (x, _, _)
  | PDGLocal (x, _, _)
  | PDGFun (x, _, _)
  | PDGConst (x, _, _, _, _) ->  x

let dest (edge : t) : int = 
  match edge with 
  | AST (_, x)
  | CG (_, x)
  | CFG (_, x, _)
  | PDGGlobal (_, x, _)
  | PDGLocal (_, x, _)
  | PDGFun (_, x, _)
  | PDGConst (_, x, _, _, _) ->  x

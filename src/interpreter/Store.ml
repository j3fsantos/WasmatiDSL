type t = (string, Literal.t) Hashtbl.t 

let init (xs_vs : (string * Literal.t) list) : t =
  let store = Hashtbl.create (List.length xs_vs) in
  List.iter (fun (x, e) -> Hashtbl.replace store x e) xs_vs; 
  store

let get (store : t) (x : string) : Literal.t option = 
  Hashtbl.find_opt store x 

let set (store : t) (x : string) (v : Literal.t) : unit = 
  Hashtbl.replace store x v

let copy (store : t) : t = 
  Hashtbl.copy store 

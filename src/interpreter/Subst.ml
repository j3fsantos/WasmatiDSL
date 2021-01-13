type t = (string, Literal.t) Hashtbl.t

let init (xs_es : (string * Literal.t) list) : t =
  let subst = Hashtbl.create (List.length xs_es) in
  List.iter (fun (x, e) -> Hashtbl.replace subst x e) xs_es; 
  subst

let get (subst : t) (x : string) : Literal.t option = 
  Hashtbl.find_opt subst x 
 
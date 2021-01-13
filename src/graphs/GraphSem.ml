type t = (string, (Graph.t -> Literal.t list -> Literal.t)) Hashtbl.t 

let (sem : t) = Hashtbl.create 0

let functions (g : Graph.t) : Literal.t =
  let mod_id = g.mod_id in 
  let mod_edges = g.edges.(mod_id) in 
  Printf.printf "Functions: %s\n" 
    (String.concat ", " 
      (List.map
        (fun e -> string_of_int (Edge.dest e)) 
        mod_edges)); 
  List (List.map (fun e -> Literal.Int (Edge.dest e)) mod_edges)

let instructions (g : Graph.t) (args : Literal.t list) : Literal.t = 
  match args with 
  | [ Literal.Int f_ids ] -> 
      let pred (n : Node.t) : bool = 
        match n with 
        | Instruction _ -> true 
        | _ -> false in  
      let nodes = Graph.bfs g pred f_ids in 
      Printf.printf "Instructions: %s\n" 
        (String.concat ", " 
        (List.map
          (fun n -> string_of_int (Node.id n)) 
          nodes)); 
      List (List.map (fun n -> Literal.Int (Node.id n)) nodes) 
  | _ -> raise (Failure "Wrong arguments to function instructions")

let instType (g : Graph.t) (args : Literal.t list) : Literal.t = 
  match args with 
  | [ Literal.Int id ] ->
    let node = Graph.get_node g id in 
    (match node with 
    | Instruction (_, instr) -> 
      let ret_type = Node.get_instr_type instr in 
      if (id = 33) then (
        Printf.printf "Inst 33 has type: %d\n" ret_type; 
      );
      Literal.Int ret_type
    | _ -> Literal.Int (-1))  
  | _ -> raise (Failure "Wrong arguments to function instType")

let label (g : Graph.t) (args : Literal.t list) : Literal.t = 
  match args with 
  | [ Literal.Int id ] ->
    let node = Graph.get_node g id in 
    (match node with 
    | Instruction (_, instr) -> 
        let lbl = Node.get_instr_label instr in 
        if (id = 33) then (
          Printf.printf "Inst 33 (%s) has label: %s\n" (Node.instr_str instr) lbl; 
        );
        Literal.Str lbl 
    | _ -> Literal.Str "")  
  | _ -> raise (Failure "Wrong arguments to function label")

(* child(node, 0, "ast")*)
let child (g : Graph.t) (args : Literal.t list) : Literal.t = 
  match args with 
  | [ Literal.Int id; Literal.Int i; Literal.Str "ast" ] -> 
      let edges = Graph.get_ast_edges g id in 
      let edges' = List.sort (fun e1 e2 -> (Edge.dest e1) - (Edge.dest e2)) edges in 
      if (i < List.length edges') then (
        Literal.Int (Edge.dest (List.nth edges' i)) 
      ) else (
        raise (Failure "Illegal index given to child")
      )
  | _ -> raise (Failure "Wrong arguments to function label")  

(* PDGEdge(child, node, "const") *)
let pdg_edge (g : Graph.t) (args : Literal.t list) : Literal.t = 
  match args with 
  | [ Literal.Int id1; Literal.Int id2; Literal.Str "const" ] -> 
      Printf.printf "child: %d, node: %d\n" id1 id2; 
      let edges = Graph.get_pdg_edges g id1 in 
      let edges' = 
        List.filter (fun e -> match (e : Edge.t) with 
            | PDGConst (_, dest, _, _, _) when dest = id2 -> true 
            | _ -> false 
          ) edges in 
      Printf.printf "number of pdg_edges: %d\n" (List.length edges');
      Literal.Bool ((List.length edges') <> 0)
  | _ -> raise (Failure "Wrong arguments to function label")  

(* vulnerability("...") *)
let vulnerability (g : Graph.t) (args : Literal.t list) : Literal.t = 
  match args with 
  | [ Literal.Str msg ] ->
    Printf.printf "Vulnerability: %s\n" msg; 
    Int 0 
  | _ -> raise (Failure "Wrong arguments to vulnerability")  

let get_fun (str : string) : (Graph.t -> Literal.t list -> Literal.t) option = 
  Hashtbl.find_opt sem str 

let setup () : unit = 
  Hashtbl.add sem "functions" (fun g args -> functions g);
  Hashtbl.add sem "instructions" instructions; 
  Hashtbl.add sem "instType" instType; 
  Hashtbl.add sem "label" label; 
  Hashtbl.add sem "child" child;
  Hashtbl.add sem "PDGEdge" pdg_edge; 
  Hashtbl.add sem "vulnerability" vulnerability



type t = { 
  nodes: Node.t array;
  edges: Edge.t list array; 
  mod_id: int 
}

let parse (str : string) : t = 
  let lines = String.split_on_char '\n' str in
  let (nodes, edges, _) = 
    List.fold_left 
      (fun (nodes, edges, b) line -> 
        Printf.printf "Parsing line: %s\n" line; 
        if b then (
          (* parsing nodes *)
          if (line = "-") then (nodes, edges, false)
            else ((Node.parse line)::nodes, edges, true) 
        ) else (
          (* parsing edges *)
          (nodes, (Edge.parse line)::edges, false)
        )
      ) 
      ([], [], true)
      lines in 
  let nodes = List.rev nodes in 
  let edges_arr = Array.make (List.length nodes) [] in 
  List.iter (fun edge ->
    let source = Edge.source edge in 
    edges_arr.(source) <- (edge :: edges_arr.(source))
  ) edges;

  (** Move to separate function *)
  let module_id = ref None in
  let i = ref 0 in 
  while !module_id = None do 
    let node = List.nth nodes !i in 
    match node with 
    | Module _ -> module_id := Some (!i)
    | _ -> i := (!i + 1)
  done;  

  match !module_id with 
    | Some mod_id -> 
      Printf.printf "Found mod_id: %d\n" mod_id; 
        { 
          nodes = Array.of_list nodes; 
          edges = edges_arr;
          mod_id = mod_id 
        }
    | _ -> raise (Failure "module not found")

let get_ast_edges (g : t) (id : int) : Edge.t list =
  let edges = g.edges.(id) in 
  List.filter 
    (fun edge -> 
      match (edge : Edge.t) with 
      | AST _ -> true 
      | _ -> false)
    edges 

let get_pdg_edges (g : t) (id : int) : Edge.t list =
  let edges = g.edges.(id) in 
  List.filter 
    (fun edge -> 
      match (edge : Edge.t) with 
      | PDGGlobal _ | PDGLocal _ | PDGConst _ | PDGFun _ -> true 
      | _ -> false)
    edges 

(* Graph.bfs g pred f_ids in *)
let bfs (g : t) (pred : Node.t -> bool) (start : int) : Node.t list = 
  let queue = Queue.create () in 
  let ret = ref [] in 
  Queue.add start queue; 

  while not (Queue.is_empty queue) do 
    let curr = Queue.take queue in 
    let node = g.nodes.(curr) in 
    if (pred node) then (
      ret := node::(!ret)
    ); 
    let edges = get_ast_edges g curr in 
    List.map (fun e -> Queue.add (Edge.dest e) queue) edges; 
    ()  
  done;

  !ret 

let get_node (g : t) (i : int) : Node.t = 
  g.nodes.(i)
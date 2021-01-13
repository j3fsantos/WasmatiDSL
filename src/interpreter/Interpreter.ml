

let rec eval_expr (store : Store.t) (graph : Graph.t) (expr : Expr.t) : Literal.t = 
  match expr with 
  | Lit l    -> l 
  
  | Var x    -> 
    (match Store.get store x with 
    | None -> raise (Failure "Unbound variable")
    | Some v -> v)
  
  | EList es -> Literal.List (List.map (eval_expr store graph) es)
  
  | BinOp (op, e1, e2) -> 
    let v1 = eval_expr store graph e1 in 
    let v2 = eval_expr store graph e2 in
    BinOp.eval op v1 v2 
  
  | UnOp (op, e) -> UnOp.eval op (eval_expr store graph e)
  
  (* [| e | x in d_e : p_e |] *)
  | RangeExpr (r_e, x, d_e, p_e) ->  
    let v = eval_expr store graph d_e in 
    List 
      (match v with 
        | List vs -> 
          List.fold_left
            (fun vs v -> 
              let subst = Subst.init [ (x, v) ] in 
              let e' = Expr.subst subst p_e in 
              let v' = eval_expr store graph e' in 
              match v' with 
              | Bool true ->
                let e'' = Expr.subst subst r_e in 
                let v'' = eval_expr store graph e'' in 
                v'' :: vs 
              | Bool false -> vs 
              | _ -> Printf.printf "Range predicate did not evaluate to a boolean"; vs)
            [] vs
        | _ -> [])
  
  | Call (f, es) -> 
    let f' = GraphSem.get_fun f in 
    (match f' with 
      | Some f'' -> 
          let vs = List.map (eval_expr store graph) es in 
          f'' graph vs 
      | None ->
          let msg = Printf.sprintf "Non implemented Function %s" f in  
          raise (Failure msg))
  
  | _ -> raise (Failure "Non-supported expression")


let rec eval (store : Store.t) (graph : Graph.t) (stmt : Stmt.t) : unit = 
  match stmt with 
    | ForEach (x, e, s) -> 
      let v = eval_expr store graph e in 
      (match v with 
        | List lst -> 
          List.iter (fun v ->
            Store.set store x v;
            eval store graph s        
          ) lst
        | _ -> Printf.printf "WARNING: ranging value of for-each statement is not a list")
    
    | Asgn (x, e) -> 
      let v = eval_expr store graph e in 
      Printf.printf "obtained e: %s\n" (Literal.str v); 
      Store.set store x v 
    
    | If (e, s1, s2) -> 
      let b = eval_expr store graph e in 
      (match b, s2 with 
        | Bool true, _ -> eval store graph s1 
        | Bool false, Some s2 -> eval store graph s2 
        | Bool false, None -> () 
        | _ -> Printf.printf "WARNING: if guard is not a boolean")
    
    | Block stmts -> List.iter (eval store graph) stmts

    | ExprStmt e -> eval_expr store graph e; ()
        
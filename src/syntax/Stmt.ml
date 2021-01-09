type t = 
    | ForEach  of string * Expr.t * t 
    | Asgn     of string * Expr.t 
    | If       of Expr.t * t * t option  
    | Block    of t list 
    | ExprStmt of Expr.t  

let rec to_json (s : t) : string = 
    let fs ss = String.concat ", " (List.map to_json ss) in 
    match s with 
    | ForEach (x, e, s) ->
        Printf.sprintf "{ \"type\": \"FOR_EACH\", \"var\": \"%s\", \"expr\": %s, \"stmt\": %s}" 
            x (Expr.to_json e) (to_json s)
    | Asgn (x, e) -> 
        Printf.sprintf "{ \"type\": \"ASGN\", \"var\": \"%s\", \"expr\": %s}" 
            x (Expr.to_json e) 
    | If (e, s, None) -> 
        Printf.sprintf "{ \"type\": \"IF\", \"expr\": %s, \"then\": %s}" 
            (Expr.to_json e) (to_json s)
    | If (e, s1, Some s2) -> 
        Printf.sprintf "{ \"type\": \"IF\", \"expr\": %s, \"then\": %s, \"else\": %s}" 
            (Expr.to_json e) (to_json s1) (to_json s2)
    | Block ss ->
        Printf.sprintf "{ \"type\": \"BLOCK\", \"stmts\": [%s]}" (fs ss)
     | ExprStmt e -> 
        Printf.sprintf "{ \"type\": \"EXPR_STMT\", \"expr\": %s}" 
            (Expr.to_json e) 
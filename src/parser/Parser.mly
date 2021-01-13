%token IN
%token AND
%token NOT
%token FOREACH
%token ASGN
%token IF
%token DO
%token THEN
%token ELSE
%token END
%token NIL
%token EQUAL

%token DOT
%token COMMA
%token COLON
%token SCOLON
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token CRBRACKET
%token CLBRACKET
%token VERT 
%token EOF

%token <string> VAR
%token <string> STRING
%token <float> FLOAT
%token<int> INT

%type <Stmt.t> prog_target
%type <Stmt.t> stmt_target
%type <Expr.t> expr_target
%start prog_target

%% (* separator line *)

prog_target: 
    | s=stmt_target; EOF { s }

stmt_target:
    (* foreach x in e do stmt end *)
    | FOREACH; x=VAR; IN; e=expr_target; DO; s=stmt_target; END 
      { Stmt.ForEach (x, e, s) }
    (* x := e *)
    | x=VAR; ASGN; e=expr_target
      { Stmt.Asgn (x, e) }
    (* if e then s end *)
    | IF; e=expr_target; THEN; s=stmt_target; END
      { Stmt.If (e, s, None) }
    (* if e then s1 else s2 end *)
    | IF; e=expr_target; THEN; s1=stmt_target; ELSE; s2=stmt_target; END     
      { Stmt.If (e, s1, Some s2) }
    (* s1; s2; ... *)
    | CLBRACKET; ss = separated_list (SCOLON, stmt_target); CRBRACKET
      { Stmt.Block ss }
    (* e *)
    | e=expr_target 
      { Stmt.ExprStmt e }

expr_target: 
    (* [| e1 | x in e2 : e3 |] *)
    | LBRACKET; VERT; e1=expr_target; VERT; x=VAR; IN; e2=expr_target; COLON; e3=expr_target; VERT; RBRACKET 
      { Expr.RangeExpr (e1, x, e2, e3) }
    (* e.x *)
    | e=expr_target; DOT; x=VAR
      { Expr.Call (x, [ e ]) }
    (* e1 bop e2 *)
    | e1=expr_target; bop=bop_target; e2=expr_target 
      { Expr.BinOp (bop, e1, e2) }
    (* uop e *)
    | uop=uop_target; e=expr_target 
      { Expr.UnOp (uop, e) }
    (* x *)
    | x = VAR
      { Expr.Var x }
    (* lit *)
    | l = lit_target 
      { Expr.Lit l }
    (* x(e1, ..., en) *) 
    | x=VAR; LBRACE; es=separated_list (COMMA, expr_target); RBRACE 
      { Expr.Call (x, es) }
    (* [e1, ..., en] *)
    | LBRACKET; es=separated_list (COMMA, expr_target); RBRACKET 
      { Expr.EList es }
    (* (e) *)
    | LBRACE; e=expr_target; RBRACE
      { e }

bop_target: 
    | EQUAL { BinOp.Equal }
    | IN    { BinOp.In    }
    | AND   { BinOp.And   }

uop_target:
    | NOT   { UnOp.Not    }

lit_target:
  | f = FLOAT  { Literal.Flt f }
  | i = INT    { Literal.Int i }
  | s = STRING { Literal.Str s }





type instr_t =
    (* id, instr_type *)
  | SimpleInstr of int 
    (* id, wasm_type, wasm_val *)
  | ConstInstr of Wasm.type_t * Wasm.val_t
    (* id, instr_type, op_code *)
  | OpCodeInstr of int * string 
    (* id, instr_type, op_code, offset *)
  | LoadStore of int * string * int 
    (* id, instr_type, label *)
  | LblInstr of int * string 
    (* id, instr_type, label, n_args, n_results *)
  | CallInstr of int * string * int * int 
    (* id, instr_type, label, n_results *)
  | BlockBase of int * string * int
    (* id, instr_type, n_results, has_else *)
  | If of int * int * bool 

(* funcoes sao filhas imediatas do modulo *)
type t =
    (* 0: id, name *)
  | Module of int * string 
    (* 1: id, name, index, nargs, nlocals, nresults, isImport, isExport *)
  | Function of int * string * int * int * int * int * bool * bool 
    (* 2: id, index, name, var_type *)
  | Var of int * int * string * Wasm.type_t 
    (* 3: id *)
  | FuncSignature of int 
    (* 4: id *)
  | Instructions of int 
    (* 5: ... *)
  | Instruction of int * instr_t
    (* 6: id *)
  | Parameters of int 
    (* 7: id *)
  | Locals of int
    (* 8: results *)
  | Results of int 
    (* 9: id *)
  | Return of int
    (* 10: else *)
  | Else of int 
    (* 11: trap *)
  | Trap of int 
    (* 12: start *)
  | Start of int 

let int_idx (elems : string list) (idx : int) : int =
  int_of_string (List.nth elems idx) 

let bool_idx (elems : string list) (idx : int) : bool =
  (int_of_string (List.nth elems idx)) <> 0 
  
let parse_module (elems : string list) : t = 
  let id = int_idx elems 0 in 
  let name = List.nth elems 2 in 
    Module (id, name)

let parse_function (elems : string list) : t = 
  Printf.printf "parsing a function\n"; 
  let id        = int_idx elems 0 in 
  let name      = List.nth elems 2 in 
  let index     = int_idx elems 3 in 
  let nargs     = int_idx elems 4 in 
  let nlocals   = int_idx elems 5 in 
  let nresults  = int_idx elems 6 in 
  let is_import = bool_idx elems 7 in 
  let is_export = bool_idx elems 8 in  
    Function (id, name, index, nargs, nlocals, nresults, is_import, is_export)

let parse_varnode (elems : string list) : t = 
  let id        = int_idx elems 0 in 
  let name      = List.nth elems 2 in 
  let index     = int_idx elems 3 in 
  let vtype     = Wasm.parse_t (List.nth elems 9) in 
    Var (id, index, name, vtype)

let parse_common (f : int -> t) (elems : string list) : t =
  Printf.printf "Inside parse common\n"; 
  let id = int_idx elems 0 in 
    f id 

let parse_simple_instr (elems : string list) : instr_t * int = 
  let id        = int_idx elems 0 in   
  let inst_type = int_idx elems 10 in 
    SimpleInstr inst_type, id 

let parse_const_instr (elems : string list) : instr_t * int = 
  let id               = int_idx elems 0 in   
  let (wasm_t, wasm_v) = Wasm.parse_vt (List.nth elems 12) (List.nth elems 13) (List.nth elems 14) in 
    ConstInstr (wasm_t, wasm_v), id 

let parse_opcode_instr (elems : string list) : instr_t * int = 
  let id        = int_idx elems 0 in   
  let inst_type = int_idx elems 10 in 
  let opcode    = List.nth elems 11 in 
    OpCodeInstr (inst_type, opcode), id 

let parse_load_store (elems : string list) : instr_t * int = 
  Printf.printf "parse_load_store\n"; 
  let id        = int_idx elems 0 in   
  let inst_type = int_idx elems 10 in 
  let opcode    = List.nth elems 11 in 
  let offset    = int_idx elems 15 in 
    LoadStore (inst_type, opcode, offset), id 

let parse_lbl_instr (elems : string list) : instr_t * int = 
  let id        = int_idx elems 0 in   
  let inst_type = int_idx elems 10 in 
  let label     = List.nth elems 15 in 
    LblInstr (inst_type, label), id 

let parse_call_instr (elems : string list) : instr_t * int = 
  let id        = int_idx elems 0 in   
  let inst_type = int_idx elems 10 in 
  let label     = List.nth elems 14 in 
  let nargs     = int_idx elems 4 in 
  let nresults  = int_idx elems 6 in 
    Printf.printf "parse_call_instr %s\n" label;  
    CallInstr (inst_type, label, nargs, nresults), id 

let parse_block_instr (elems : string list) : instr_t * int = 
  let id        = int_idx elems 0 in   
  let inst_type = int_idx elems 10 in 
  let label     = List.nth elems 15 in 
  let nresults  = int_idx elems 6 in 
    BlockBase (inst_type, label, nresults), id 

let parse_if_instr (elems : string list) : instr_t * int = 
  let id        = int_idx elems 0 in   
  let inst_type = int_idx elems 10 in 
  let nresults  = int_idx elems 6 in 
  let haselse   = bool_idx elems 17 in 
    If (inst_type, nresults, haselse), id 

let parse_instruction (elems : string list) : t = 
  Printf.printf "Inside parse isntruction with %s\n" (String.concat ", " elems); 
  let parser = [ 
    parse_simple_instr; (* nop *) 
    parse_simple_instr; (* unreachable *) 
    parse_simple_instr; (* return *)
    parse_simple_instr; (* branch table *)
    parse_simple_instr; (* drop *)
    parse_simple_instr; (* select *)
    parse_simple_instr; (* memory size *)
    parse_simple_instr; (* memory grow *)
    parse_const_instr; 
    parse_opcode_instr; (* binary *)
    parse_opcode_instr; (* compare *)
    parse_opcode_instr; (* convert *)
    parse_opcode_instr; (* unary *)
    parse_load_store;   (* load *)
    parse_load_store;   (* store *)
    parse_lbl_instr;    (* branch *)
    parse_lbl_instr;    (* branch if *)
    parse_lbl_instr;    (* global get *)
    parse_lbl_instr;    (* global set *)
    parse_lbl_instr;    (* local get *)
    parse_lbl_instr;    (* local set *)
    parse_lbl_instr;    (* local tee *)
    parse_call_instr;   (* call *)
    parse_call_instr;   (* call indirect *)
    parse_block_instr;  (* begin block *)
    parse_block_instr;  (* block *)
    parse_block_instr;  (* loop *)
    parse_if_instr;   
    parse_block_instr  (* end loop *)
  ] in 
  let idx = int_idx elems 10 in 
  let f = List.nth parser idx in
  let (instr, id) = f elems in 
    Instruction (id, instr)  

let parse (str : string) : t = 
  Printf.printf "Inside parse with %s\n" str; 
  let elems = String.split_on_char ',' str in 
  let parser = [
    parse_module; 
    parse_function; 
    parse_varnode; 
    parse_common (fun i -> FuncSignature i); 
    parse_common (fun i -> Instructions i);
    parse_instruction;  
    parse_common (fun i -> Parameters i); 
    parse_common (fun i -> Locals i); 
    parse_common (fun i -> Results i); 
    parse_common (fun i -> Return i); 
    parse_common (fun i -> Else i); 
    parse_common (fun i -> Trap i); 
    parse_common (fun i -> Start i) 
  ] in 
  let idx = int_idx elems 1 in 
  let f = List.nth parser idx in 
    f elems 


let id (node : t) : int = 
  match node with 
  | Module (x, _)
  | Function (x, _, _, _, _, _, _, _)
  | Var (x, _, _, _)
  | FuncSignature x
  | Instructions x 
  | Instruction (x, _)
  | Parameters x 
  | Locals x 
  | Results x 
  | Return x 
  | Else x 
  | Trap x 
  | Start x -> x


let get_instr_type (instr : instr_t) : int = 
  match instr with 
  | SimpleInstr x 
  | OpCodeInstr (x, _)
  | LoadStore (x, _, _)
  | LblInstr (x, _)
  | CallInstr (x, _, _, _)
  | BlockBase (x, _, _)
  | If (x, _, _) -> x 
  | ConstInstr _ -> 8 (** Oh my god *)

let get_instr_label (instr : instr_t) : string = 
  match instr with 
  | LblInstr (_, lbl)
  | CallInstr (_, lbl, _, _)
  | BlockBase (_, lbl, _) -> lbl 
  | _ -> ""


let instr_str (instr : instr_t) : string = 
  match instr with 
  | SimpleInstr _ -> "simple instr" 
  | OpCodeInstr _ -> "op code instr" 
  | LoadStore _ -> "load instr"
  | LblInstr _ -> "lbl instr"
  | CallInstr (x, lbl, i, j) -> Printf.sprintf "call (%d, %s, %d, %d)" x lbl i j 
  | BlockBase _ -> "block base"
  | If _ -> "if" 
  | ConstInstr _ -> "const"
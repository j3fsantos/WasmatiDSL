exception ImportException of string

type token = [%import: Parser.token] [@@deriving show]

let print_position
    (outx : Format.formatter)
    (lexbuf : Lexing.lexbuf) : unit =
  let pos = lexbuf.lex_curr_p in
  Printf.printf "Line number: %d. File: %s\n" pos.pos_lnum pos.pos_fname;  
  Format.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) 


let parse start (lexbuf : Lexing.lexbuf) =
  let module MIM = Parser.MenhirInterpreter in

  let last_token = ref Parser.EOF
  in let lexer lexbuf =
       let token = Lexer.read lexbuf in
       last_token := token; token in

  MIM.loop_handle
    (fun result -> result)   
    (function MIM.Rejected -> failwith "Parser rejected input"
            | MIM.HandlingError e ->
              (* let csn = ESLMI.current_state_number e in *)
              Format.eprintf "%a, last token: %s: %s.@."
                print_position lexbuf
                (show_token !last_token)
                "Error message found";
              raise Parser.Error
            | _ -> failwith "Unexpected state in failure handler!")
    (MIM.lexer_lexbuf_to_supplier lexer lexbuf)
    (start lexbuf.Lexing.lex_curr_p)

let init_lexbuf (str : string) =
  let lexbuf = Lexing.from_string str in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = "one_file" };
  lexbuf 


let parse_stmt (str : string) : Stmt.t =
  Printf.printf "parse_stmt\n";
  let lexbuf = init_lexbuf str in 
  let s = parse Parser.Incremental.prog_target lexbuf in
  s


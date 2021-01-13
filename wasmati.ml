open Interpreter

let ifile = ref ""
let ofile = ref ""
let gfile = ref ""

let burn_to_disk (path : string) (data : string) (msg : string) : unit =
  if (path <> "") then ( 
    let oc = open_out path in
    output_string oc data;
    close_out oc
  ) else (
    Printf.printf "%s\n" msg
  )

let load_file (f : string) (msg : string) : string =
  if f = "" then raise (Failure msg) else ( 
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s)


let arguments () =
  let usage_msg = "Usage: -i <path> -mode <c/p> -o <path> [-v] -h <path> [--parse]" in
  Arg.parse
    [
      ("-q", Arg.String (fun f -> ifile := f), "Input Query file");
      ("-o", Arg.String (fun f -> ofile := f), "Output file"); 
      ("-g", Arg.String (fun f -> gfile := f), "Input Graph")
    ]
    (fun s -> Printf.printf "Ignored Argument: %s" s)
    usage_msg


let run () : unit  =
    arguments();
    let f_contents = load_file !ifile "WARNING: No query was supplied" in
    Printf.printf "file:\n%s\n" f_contents; 
    let stmt = ParsingUtils.parse_stmt f_contents in
    let json = Stmt.to_json stmt in 
    burn_to_disk !ofile json "WARNING: No query was supplied";
    Printf.printf "filed parsed successfully:\n%s\n" json; 
    let graph_contents = load_file !gfile "WARNING: No graph was supplied" in
    let graph = Graph.parse graph_contents in 
    Printf.printf "graph parsed successfully\n";  
    let store = Store.init [] in 
    GraphSem.setup (); 
    Interpreter.eval store graph stmt 

let _ = run ()

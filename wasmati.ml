open ParsingUtils

let ifile = ref ""
let ofile = ref ""

let burn_to_disk (path : string) (data : string) : unit =
  if (path <> "") then ( 
    let oc = open_out path in
    output_string oc data;
    close_out oc
  )

let load_file f : string =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s


let arguments () =
  let usage_msg = "Usage: -i <path> -mode <c/p> -o <path> [-v] -h <path> [--parse]" in
  Arg.parse
    [
      ("-i", Arg.String (fun f -> ifile := f), "Input file");
      ("-o", Arg.String (fun f -> ofile := f), "Output file")
    ]
    (fun s -> Printf.printf "Ignored Argument: %s" s)
    usage_msg


let run () : unit  =
    arguments();
    let f_contents = load_file !ifile in
    Printf.printf "file:\n%s\n" f_contents; 
    let s = ParsingUtils.parse_stmt f_contents in
    let json = Stmt.to_json s in 
    burn_to_disk !ofile json;
    Printf.printf "filed parsed successfully:\n%s\n" json


let _ = run ()

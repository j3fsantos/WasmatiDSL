{
  open Lexing  

  exception Syntax_error of string
}

let digit = ['0'-'9']
let letter = ['a'-'z''A'-'Z']
let identifier = letter(letter|digit|'_')*
let float = '-'? digit+ ('.' digit*)?
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let var     = (letter | '_'*letter)(letter|digit|'_'|'\'')*
let int     = '-'?digit+

rule read = parse
  | white                { read lexbuf }
  | newline              { new_line lexbuf; read lexbuf }
  (* keywords *)
  | "="                  { Parser.EQUAL         }
  | "in"                 { Parser.IN            }
  | "&&"                 { Parser.AND           }
  | "!"                  { Parser.NOT           }
  | "foreach"            { Parser.FOREACH       }
  | ":="                 { Parser.ASGN          }
  | "if"                 { Parser.IF            }
  | "do"                 { Parser.DO            }
  | "then"               { Parser.THEN          }
  | "else"               { Parser.ELSE          }
  | "end"                { Parser.END           }
  | "nil"                { Parser.NIL           }
  (* separators *)
  | '.'                  { Parser.DOT       }
  | ','                  { Parser.COMMA     }
  | ':'                  { Parser.COLON     }
  | ';'                  { Parser.SCOLON    }
  | '('                  { Parser.LBRACE    }
  | ')'                  { Parser.RBRACE    }
  | '['                  { Parser.LBRACKET  }
  | ']'                  { Parser.RBRACKET  }
  | '{'                  { Parser.CLBRACKET }
  | '}'                  { Parser.CRBRACKET }
  | '|'                  { Parser.VERT }
  (* variables *)
  | var                  { VAR (Lexing.lexeme lexbuf) }
  (* literals *)
  | float                { Parser.FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | int                  { Printf.printf "int"; Parser.INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '"'                  { read_string (Buffer.create 17) lexbuf }
  | eof                  { EOF }

and read_string buf = parse
  | '"'                  { STRING (Buffer.contents buf) }
  | [^ '"' '\\']+        {
                           Buffer.add_string buf (Lexing.lexeme lexbuf);
                           read_string buf lexbuf
                         }
  | eof                  { raise (Syntax_error ("String is not terminated")) }




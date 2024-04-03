(* Ocamllex scanner for Cesame *)

{ open Parser }

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let heading_spaces = ('\r' | '\n' | "\r\n") [' ' '\t']*

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { gcomment lexbuf }          (* General Comments *)
| "//"     { lcomment lexbuf }          (* Line Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
(* COMMA *)
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "&&"     { AND }
| "||"     { OR }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
(* RETURN *)
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "String" { STRING }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "continue" { CONTINUE }
| "break" { BREAK }
| "for" { FOR }
| "Func" { FUNC }
| "->" { ARROW }
| '"'      { let s = "" in strparse s lexbuf }
| digit+ as lem  { LITERAL(int_of_string lem) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and gcomment = parse
  "*/" { token lexbuf }
| _    { gcomment lexbuf }
and lcomment = parse 
  '\n' { token lexbuf }
| _    { lcomment lexbuf }

and strparse s = parse
  '"'  { STRLIT(Scanf.unescaped s)}
| heading_spaces { strparse s lexbuf }
| eof  { raise (Failure("unexpected end of string")) }
| _ as c { strparse (s ^ (String.make 1 c)) lexbuf }

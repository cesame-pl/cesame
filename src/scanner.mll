(* Ocamllex scanner for Cesame *)

{ open Parser }

let squote = '\''
let letter = ['a'-'z' 'A'-'Z']
let heading_spaces = ('\r' | '\n' | "\r\n") [' ' '\t']*

let exp = ('e'|'E')
let sign = ('+'|'-')
let digit = ['0'-'9']
let int = sign? digit+
let float = (
  int? '.' digit+ (exp int)? |
  int '.' digit* (exp int)? |
  int exp int
)

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { gcomment lexbuf }          (* General Comments *)
| "//"     { lcomment lexbuf }          (* Line Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LSQBRACE }
| ']'      { RSQBRACE }
| ';'      { SEMI }
(* COMMA *)
| ','      { COMMA }
| '*'      { MUL }
| '/'      { DIV }
| '%'      { MOD }
| '+'      { PLUS }
| '-'      { MINUS }
| '='      { ASSIGN }
| "++"     { INC }
| "--"     { DEC }
| '!'      { NOT }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| '>'      { GT }
| ">="     { GE }
| "<="     { LE }
| "&&"     { AND }
| "||"     { OR }
(* IF...ELIF...ELSE *)
| "if"     { IF }
| "else"   { ELSE }
| "elif"   { ELIF }
| "while"  { WHILE }
(* RETURN *)
| "return" { RETURN }
| "int"    { INT }
| "char"   { CHAR }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "String" { STRING }
| "Array"  { ARRAY }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| "continue" { CONTINUE }
| "break"  { BREAK }
| "for"    { FOR }
| "Func"   { FUNC }
| "->"     { ARROW }
| '"'      { let s = "" in strparse s lexbuf }
| int as lem  { LITERAL(int_of_string lem) }
| float as lem { FLIT(float_of_string lem) }
| squote _ squote as lem { CLIT(lem.[1]) }
| squote '\\' ('n' | 't' | '\\' | '\'') squote as lem { 
  let c = match lem.[2] with
          | 'n' -> '\n'
          | 't' -> '\t'
          | '\\' -> '\\'
          | '\'' -> '\''
  in CLIT(c)  }
| letter (digit | letter | '_')* as lem { ID(lem) }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
| eof       { EOF }

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

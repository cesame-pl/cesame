(* Ocamllex scanner for Cesame *)

{ open Parser }

let squote = '\''
let lletter = ['a'-'z']
let uletter = ['A'-'Z']
let letter = lletter | uletter
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
| ','      { COMMA }
(* Operators *)
| "++"     { INC }
| "--"     { DEC }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { MUL }
| '/'      { DIV }
| '%'      { MOD }
| "=="     { EQ }
| "!="     { NEQ }
| '!'      { NOT } (* Not(!) needs to go after NEQ(!=) *)
| ">="     { GE }
| "<="     { LE }
| '<'      { LT }
| '>'      { GT }
| "&&"     { AND }
| "||"     { OR }
| '='      { ASSIGN } (* Assign(=) needs to go after EQ(==) *)
(* Control Flow *)
| "if"     { IF }
| "else"   { ELSE }
| "elif"   { ELIF }
| "for"    { FOR }
| "while"  { WHILE }
| "continue" { CONTINUE }
| "break"  { BREAK }
| "return" { RETURN }
(* Types *)
| "int"    { INT }
| "char"   { CHAR }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "String" { STRING }
| "struct" { STRUCT }
| "Array"  { ARRAY }
| "Func"   { FUNC }
| "->"     { ARROW }
| "new"    { NEW }
| "delete" { DELETE }
(* ID *)
| uletter (digit | letter | '_')* as lem { STRUCTID(lem) }
| lletter (digit | letter | '_')* as lem { ID(lem) }
(* Literals *)
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| int as lem  { LITERAL(int_of_string lem) }
| float as lem { FLIT(float_of_string lem) }
(* dots that are not in between numbers are parsed just as a single dot*)
| "."      { DOT }
| squote _ squote as lem { CLIT(lem.[1]) }
| squote '\\' ('n' | 't' | '\\' | '\'') squote as lem { 
  let c = match lem.[2] with
          | 'n' -> '\n'
          | 't' -> '\t'
          | '\\' -> '\\'
          | '\'' -> '\''
  in CLIT(c)  }
| '"'      { let s = "" in strparse s lexbuf }

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

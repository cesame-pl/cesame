/* Ocamlyacc parser for Cesame */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE MUL DIV MOD PLUS MINUS ASSIGN INC DEC LSQBRACE RSQBRACE
%token CONTINUE BREAK FOR FUNC ARROW
%token NOT GE LE GT LT EQ NEQ AND OR
%token IF ELIF ELSE WHILE INT CHAR BOOL FLOAT STRUCT ARRAY
%token RETURN COMMA
%token STRING
%token NEW DELETE
%token DOT
%token <int> LITERAL
%token <char> CLIT
%token <bool> BLIT
%token <float> FLIT
%token <string> ID
%token <string> STRUCTID
%token <string> STRLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%nonassoc INC DEC
%right ASSIGN
%right NOT
%left OR
%left AND
%left EQ NEQ
%left GE LE GT LT
%left MUL DIV MOD PLUS MINUS

%%

/* add function declarations*/
program:
  stmt_list EOF { $1 }

// decls:
//   /* nothing */      { ([], [])                 }
//   | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
//   | fdecl decls      { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /* nothing */            { [] }
  | vdecl SEMI vdecl_list  { $1 :: $3 }

/* int x */
/* TODO: support int x = 1; */
vdecl:
  typ ID { ($1, $2) } (* of type "bind", for function definition and vdecl outside of function *)

typ:
    INT   { Int   }
  | CHAR  { Char  }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | STRING { String }
  | STRUCTID { Struct $1 }
  | ARRAY LT typ GT { Array $3 }

/* fdef */
fdef:
  vdecl LPAREN params_opt RPAREN LBRACE stmt_list RBRACE
  {
    FDef({
      rtyp=fst $1;
      fname=snd $1;
      params=$3;
      body=$6
    })
  }

/* params_opt */
params_opt:
  /* nothing */  { [] }
  | params_list { $1 }

params_list:
    vdecl { [$1] }
  | vdecl COMMA params_list { $1::$3 }

stmt_list:
  /* nothing */     { [] }
  | stmt stmt_list  { $1::$2 }

/* TODO: It might be better to seperate out some important clause, like vdecl */
stmt:
    expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  | typ ID ASSIGN expr SEMI                 { VDecl($1, $2, Some($4)) }
  | typ ID SEMI                             { VDecl($1, $2, None) }
  | STRUCT STRUCTID LBRACE vdecl_list RBRACE SEMI { SDef($2, $4) }
  /* if (condition) { block1 } else { block2 } */
  /* if (condition) stmt else stmt */
  /* if (condition) stmt (elif stmt)+ NOELSE*/
  | ifelifstmt ELSE stmt                    { If($1, $3) }
  | ifelifstmt %prec NOELSE                 { If($1, Expr(Noexpr)) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  /* for statement */
  /* TODO: for i in array */
  | FOR LPAREN opt_loop_init SEMI opt_expr SEMI opt_expr RPAREN LBRACE stmt_list RBRACE
  {
    For($3, $5, $7, $10)
  }
  /* return */
  | RETURN expr SEMI                        { Return $2      }
  /* non-first class function definition*/
  | fdef                                    { $1             }
  /* delete */
  | DELETE ID                               { Delete $2      }


ifelifstmt:
    IF LPAREN expr RPAREN stmt              { [($3, $5)] }
  | ifelifstmt ELIF LPAREN expr RPAREN stmt { ($4, $6)::$1 }

expr:
    LITERAL          { Literal($1)            }
  | CLIT             { CharLit($1)            }
  | BLIT             { BoolLit($1)            }
  | FLIT             { FloatLit($1)           }
  | STRLIT           { StrLit($1)             }
  | LSQBRACE elements RSQBRACE { ArrayLit($2) }
  | lvalue           { $1                     }
  | lvalue ASSIGN expr { Assign($1, $3)       }
  | lvalue INC       { Assign($1, Binop($1, Add, Literal(1))) }
  | lvalue DEC       { Assign($1, Binop($1, Sub, Literal(1))) }
  | NOT expr         { Unaop(Not, $2)         }
  | expr MUL    expr { Binop($1, Mul,   $3)   }
  | expr DIV    expr { Binop($1, Div,   $3)   }
  | expr MOD    expr { Binop($1, Mod,   $3)   }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr GE     expr { Binop($1, Ge,    $3)   }
  | expr LE     expr { Binop($1, Le,    $3)   }
  | expr GT     expr { Binop($1, Gt,    $3)   }
  | expr LT     expr { Binop($1, Lt,    $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq,   $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | LPAREN expr RPAREN { $2                   }
  /* function call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3) }
  /* new object, like "new Student {1, 2}", or simply "new Student", although it's a expr, it should not be used alone (without being assigned to some variable) */
  | NEW STRUCTID     { New(NewStruct $2)      }

lvalue:
    ID { Id $1 }
  /* Access struct members */
  | lvalue DOT ID  { AccessMember($1, Id $3)   }
  /* Access array elements */
  | lvalue LSQBRACE expr RSQBRACE { AccessEle($1, $3) }

/* args_opt*/
args_opt:
  /* nothing */ { [] }
  | args { $1 }

elements:
  /* nothing */ { [] }
  | expr { [$1] }
  | expr COMMA elements { $1::$3 }

args:
    expr  { [$1] }
  | expr COMMA args { $1::$3 }

opt_expr:
    /*nothing*/ { None }
  | expr { Some($1) }

opt_loop_init:
    /*nothing*/ { None } /* for option */
  | expr { Some(Expr $1) }
  (* TODO: Can typ ID ASSIGN expr be expressed with some variation of vdecl *)
  | typ ID ASSIGN expr {  Some(VDecl($1, $2, Some($4))) }

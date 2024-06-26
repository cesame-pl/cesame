/* Ocamlyacc parser for Cesame */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE MUL DIV MOD PLUS MINUS ASSIGN PLUSEQ MINUSEQ MULEQ DIVEQ MODEQ INC DEC LSQBRACE RSQBRACE
%token CONTINUE BREAK FOR FUNC ARROW
%token NOT GE LE GT LT EQ NEQ AND OR
%token IF ELIF ELSE WHILE VOID CHAR BOOL INT FLOAT STRUCT ARRAY
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

/* program entry point */
program:
  sdecl_list stmt_list EOF {($1, $2)}

/* variable declarations */
vdecl_list:
  /* nothing */            { [] }
  | vdecl SEMI vdecl_list  { $1 :: $3 }
vdecl:
  typ ID { ($1, $2) } /* of type "bind", for function definition and vdecl outside of function */

/* types */
typ_list:
    /* nothing */       { []       }
  | typ                 { [$1]     }
  | typ COMMA typ_list  { $1 :: $3 }
typ:
    VOID  { Void  }
  | CHAR  { Char  }
  | BOOL  { Bool  }
  | INT   { Int   }
  | FLOAT { Float }
  | STRING { String }
  | STRUCTID { Struct $1 }
  | ARRAY LT typ GT { Array $3 }
  | FUNC LPAREN typ_list RPAREN ARROW typ 
    { Func($3, $6) }

/* function definition 1 */
fdef_prev:
  vdecl LPAREN params_opt RPAREN LBRACE stmt_list RBRACE
  {
    FDef({
      rtyp=fst $1;
      fname=snd $1;
      params=$3;
      body=$6
    })
  }
/* function definition 2 */
fdef: 
  FUNC ID LPAREN params_opt RPAREN ARROW typ LBRACE stmt_list RBRACE
  {
    FDef({
      rtyp=$7;
      fname=$2;
      params=$4;
      body=$9
    })
  }
/* anonymous function definition: fname start with empty */
anondef:
  LPAREN params_opt RPAREN ARROW typ LBRACE stmt_list RBRACE
  {
    AnonFunc({
      rtyp=$5;
      fname=""; 
      params=$2;
      body=$7;
    })
  }

/* struct declaration */
sdecl_list:
  /* nothing */  { [] }
  | sdecl sdecl_list  { $1 :: $2 }
sdecl:
  STRUCT STRUCTID LBRACE vdecl_list RBRACE SEMI { {sname = $2; body = $4} }

/* params_opt */
params_opt:
  /* nothing */  { [] }
  | params_list { $1 }
params_list:
    vdecl { [$1] }
  | vdecl COMMA params_list { $1::$3 }

/* statements */
stmt_list:
  /* nothing */     { [] }
  | stmt stmt_list  { $1::$2 }

/* TODO: It might be better to seperate out some important clause, like vdecl */
stmt:
    expr SEMI                               { Expr $1                 }
  | LBRACE stmt_list RBRACE                 { Block $2                }
  | typ ID SEMI                             { VDecl($1, $2, None)     }
  | typ ID ASSIGN expr SEMI                 { VDecl($1, $2, Some($4)) }
  /* if (condition) { block1 } else { block2 } */
  /* if (condition) stmt else stmt */
  /* if (condition) stmt (elif stmt)+ NOELSE*/
  | ifelifstmt ELSE stmt                    { If($1, $3)              }
  | ifelifstmt %prec NOELSE                 { If($1, Expr(Noexpr))    }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)          }
  /* for statement */
  /* TODO: for i in array */
  | FOR LPAREN loop_init_opt SEMI expr_opt SEMI expr_opt RPAREN LBRACE stmt_list RBRACE
    { For($3, $5, $7, $10) }
  | CONTINUE SEMI                           { Continue                }
  | BREAK SEMI                              { Break                   }
  /* non-first class function definition*/
  | fdef_prev                               { $1                      }
  | fdef                                    { $1                      }
  /* return */
  | RETURN expr SEMI                        { Return $2               }
  /* delete */
  | DELETE lvalue SEMI                      { Delete $2               }

ifelifstmt:
    IF LPAREN expr RPAREN stmt              { [($3, $5)]              }
  | ifelifstmt ELIF LPAREN expr RPAREN stmt { ($4, $6) :: $1          }

/* For struct lit */
dot_assign_list:
  /* nothing */     { [] }
  | dot_assign      {[$1]}
  | dot_assign COMMA dot_assign_list { $1 :: $3 }
dot_assign:
   DOT ID ASSIGN expr { ($2, $4) }

/* expressions */
expr:
    LITERAL          { Literal($1)                 }
  | CLIT             { CharLit($1)                 }
  | BLIT             { BoolLit($1)                 }
  | FLIT             { FloatLit($1)                }
  | STRLIT           { StrLit($1)                  }
  | LSQBRACE elements RSQBRACE { New(ArrayLit($2)) }
  | NEW LSQBRACE elements RSQBRACE { New(ArrayLit($3))        }
  | LBRACE dot_assign_list RBRACE { StructLit($2)  }
  | lvalue           { $1                          }
  | lvalue ASSIGN expr { Assign($1, $3)            }
  | lvalue PLUSEQ expr { Assign($1, Binop($1, Add, $3))       }
  | lvalue MINUSEQ expr { Assign($1, Binop($1, Sub, $3))      }
  | lvalue MULEQ expr { Assign($1, Binop($1, Mul, $3))        }
  | lvalue DIVEQ expr { Assign($1, Binop($1, Div, $3))        }
  | lvalue MODEQ expr { Assign($1, Binop($1, Mod, $3))        }
  | lvalue INC       { Assign($1, Binop($1, Add, Literal(1))) }
  | lvalue DEC       { Assign($1, Binop($1, Sub, Literal(1))) }
  | NOT expr         { Unaop(Not, $2)              }
  | expr PLUS   expr { Binop($1, Add,   $3)        }
  | MINUS expr %prec MINUS { Unaop(Neg, $2)        }  
  | expr MINUS expr { Binop($1, Sub, $3)           } 
  | expr MUL    expr { Binop($1, Mul,   $3)        }
  | expr DIV    expr { Binop($1, Div,   $3)        }
  | expr MOD    expr { Binop($1, Mod,   $3)        }
  | expr GE     expr { Binop($1, Ge,    $3)        }
  | expr LE     expr { Binop($1, Le,    $3)        }
  | expr GT     expr { Binop($1, Gt,    $3)        }
  | expr LT     expr { Binop($1, Lt,    $3)        }
  | expr EQ     expr { Binop($1, Equal, $3)        }
  | expr NEQ    expr { Binop($1, Neq,   $3)        }
  | expr AND    expr { Binop($1, And,   $3)        }
  | expr OR     expr { Binop($1, Or,    $3)        }
  | LPAREN expr RPAREN { $2                        }
  /* function call */
  /* todo: function call can be a lvalue */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)      }
  | anondef          { $1                          }

lvalue:
    ID { Id $1 }
  /* Access struct members */
  | lvalue DOT ID  { AccessMember($1, Id $3)   }
  /* Access array elements */
  | lvalue LSQBRACE expr RSQBRACE { AccessEle($1, $3) }

/* potentially: we can just let expr DOT expr and expr LSQBRACE expr RSQBRACE, and check it semantically */

expr_opt:
  /* nothing */ { None }
  | expr { Some($1) }

elements:
  /* nothing */ { [] }
  | expr { [$1] }
  | expr COMMA elements { $1 :: $3 }

/* args_opt */
args_opt:
  /* nothing */ { [] }
  | args { $1 }
args:
    expr  { [$1] }
  | expr COMMA args { $1 :: $3 }

loop_init_opt:
  /* nothing */ { None } /* for option */
  | expr { Some(Expr $1) }
  /* TODO: Can typ ID ASSIGN expr be expressed with some variation of vdecl */
  | typ ID ASSIGN expr { Some(VDecl($1, $2, Some($4))) }

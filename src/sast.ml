(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = (typ * sx)
and sx =
    Noexpr
  | SLiteral of int
  | SCharLit of char
  | SBoolLit of bool
  | SFloatLit of float
  | SId of string
  | SStrLit of string
  | SArrayLit of sexpr list
  | SUnaop of unaop * sexpr
  | SBinop of sexpr * binop * sexpr
  | SAssign of sexpr * sexpr
  (* call *)
  | SCall of string * sexpr list
  | SNew of snewable
  | SAccessMember of sexpr * sexpr
  | SAccessEle of sexpr * sexpr

and snewable = 
  SNewStruct of string

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  (* | SIf of sexpr * sstmt * sstmt *)
  | SIf of (sexpr * sstmt) list * sstmt
  | SFor of (sstmt option) * (sexpr option) * (sexpr option) * (sstmt)
  | SWhile of sexpr * sstmt
  | SVDecl of typ * string * sexpr option
  | SSDef of string * (typ * string) list
  (* return *)
  | SReturn of sexpr
  | SFDef of sfunc_def
  | SDelete of string

(* func_def: ret_typ fname formals locals body *)
and sfunc_def = {
  srtyp: typ;
  sfname: string;
  sparams: bind list;
  (* slocals: bind list; *)
  sbody: sstmt list;
}

type sprogram = sstmt list

(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SLiteral(l) -> string_of_int l
      | SCharLit(c) -> Char.escaped c
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SFloatLit(f) -> string_of_float f
      | SStrLit(s) -> String.escaped s
      | SArrayLit(a) -> let rec string_of_list a = match a with
          [] -> ""
          | [element] -> string_of_sexpr element
          | hd::tl -> (string_of_sexpr (hd)) ^ "," ^ (string_of_list (tl)) 
        in "[" ^ string_of_list a ^ "]"
      | SId(s) -> s
      | SUnaop(o, e) -> 
        string_of_unaop o ^ string_of_sexpr e
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_sexpr e2
      | SAssign(e1, e2) -> string_of_sexpr e1 ^ " = " ^ string_of_sexpr e2
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
      | SNew(n) -> string_of_snewable n
      | SAccessMember(e1, e2) -> string_of_sexpr e1 ^ "." ^ string_of_sexpr e2
      | SAccessEle(e1, e2) -> string_of_sexpr e1 ^ "[" ^ string_of_sexpr e2 ^ "]"
      | Noexpr -> ""
    ) ^ ")"

and string_of_snewable = function
  | SNewStruct(s) -> "new " ^ s

let rec string_of_sstmt = 
  (function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(l, SExpr(_,Noexpr)) -> let string_of_sif ((e, s)) =
    "if (" ^ string_of_sexpr e ^ ")\n" ^ (string_of_sstmt s)
    in String.concat ("el") (List.map string_of_sif (List.rev l))
  | SIf(l,s) -> let string_of_sif ((e, s)) =
    "if (" ^ string_of_sexpr e ^ ")\n" ^ (string_of_sstmt s)
    in String.concat ("el") (List.map string_of_sif (List.rev l)) ^ "else\n" ^ string_of_sstmt s
  | SFor(s, e1, e2, l) -> "for (" ^ string_of_sstmt_opt s ^ "; " ^ string_of_sexpr_opt e1 ^ "; " ^ string_of_sexpr_opt e2 ^ ")" ^ string_of_sstmt l
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SVDecl(t, s, e) -> string_of_typ t ^ " " ^ s ^ " " ^ (let string_of_expr_option s = match s with None -> "" | Some sexp -> string_of_sexpr sexp in string_of_expr_option e) ^ ";\n"
  | SSDef(s, l) -> string_of_stmt(SDef(s, l))
  | SFDef(f) -> string_of_sfdecl f
  | SDelete(s) -> "delete " ^ s
  )
and string_of_sstmt_opt = function
  Some sstmt -> string_of_sstmt sstmt
| None -> ""
and string_of_sexpr_opt = function
  Some sexpr -> string_of_sexpr sexpr
| None -> ""
and string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sparams) ^
  ")\n{\n" ^
  (* String.concat "" (List.map string_of_vdecl fdecl.slocals) ^ *)
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram sstmts =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_sstmt sstmts)
  

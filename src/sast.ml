(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = (typ * sx)
and sx =
    Noexpr
  | SCharLit of char
  | SBoolLit of bool
  | SLiteral of int
  | SFloatLit of float
  | SStrLit of string
  | SStructLit of (string * sexpr) list
  | SNew of snewable 
  | SAnonFunc of sfunc_def
  | SId of string
  | SUnaop of unaop * sexpr
  | SBinop of sexpr * binop * sexpr
  | SAssign of sexpr * sexpr
  (* call *)
  | SCall of string * sexpr list
  | SAccessMember of sexpr * sexpr
  | SAccessEle of sexpr * sexpr (* The second sexpr can be only be of int type, so can be an expr *)

and snewable = 
  SArrayLit of sexpr list

and sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  (* | SIf of sexpr * sstmt * sstmt *)
  | SIf of (sexpr * sstmt) list * sstmt
  | SFor of (sstmt option) * (sexpr option) * (sexpr option) * (sstmt)
  | SWhile of sexpr * sstmt
  | SVDecl of typ * string * sexpr option
  | SDelete of sexpr
  | SFDef of sfunc_def
  | SReturn of sexpr
  | SBreak
  | SContinue

(* func_def: ret_typ fname params body *)
and sfunc_def = {
  srtyp: typ;
  sfname: string;
  sparams: bind list;
  sbody: sstmt list;
}

type sstruct_decl = {
  ssname: string;
  sbody: bind list;
}

type sprogram = sstruct_decl list * sstmt list

(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ 
  (match e with
    Noexpr -> ""
  | SCharLit (c) -> "'" ^ Char.escaped c ^ "'"
  | SBoolLit (true) -> "true"
  | SBoolLit (false) -> "false"
  | SLiteral (l) -> string_of_int l
  | SFloatLit (f) -> string_of_float f
  | SStrLit (s) -> "\"" ^ String.escaped s ^ "\""
  | SStructLit (assign_list) -> 
    "{ " ^ String.concat ", " (List.map string_of_sdot_assign assign_list) ^ " }"
  | SNew(SArrayLit (a)) ->
    let rec string_of_list a = match a with
        [] -> ""
      | [element] -> string_of_sexpr element
      | hd :: tl -> (string_of_sexpr hd) ^ ", " ^ (string_of_list tl) 
    in "[" ^ string_of_list a ^ "]"
  | SAnonFunc (f) ->
    "(" ^ String.concat ", " (List.map snd f.sparams) ^ ") -> " ^ 
    (string_of_typ f.srtyp) ^ " " ^ remove_last (string_of_sstmt(SBlock(f.sbody)))
  | SId (s) -> s
  | SUnaop (o, e) -> string_of_unaop o ^ string_of_sexpr e
  | SBinop (e1, o, e2) -> string_of_sexpr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_sexpr e2
  | SAssign (e1, e2) -> string_of_sexpr e1 ^ " = " ^ string_of_sexpr e2
  | SCall (f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SAccessMember (e1, e2) -> string_of_sexpr e1 ^ "." ^ string_of_sexpr e2
  | SAccessEle (e1, e2) -> string_of_sexpr e1 ^ "[" ^ string_of_sexpr e2 ^ "]") ^ 
  ")"

and string_of_sdot_assign = function
| (l, r) ->  "." ^ l ^ " = " ^ (string_of_sexpr r)

and string_of_sstmt = function
    SBlock (sstmts) -> "{\n" ^ string_of_sstmt_list sstmts ^ "}\n"
  | SExpr (e)       -> string_of_sexpr e ^ ";\n"
  | SIf (l, s)      -> 
    let string_of_sif (e, s) = "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s in 
      String.concat "el" (List.map string_of_sif (List.rev l)) ^
      (match s with SExpr(_, Noexpr) -> "" | _ -> "else\n" ^ string_of_sstmt s)
  | SFor (stmt_init, e_cond, e_trans, stmt_l) -> 
    "for (" ^ remove_last (string_of_opt_sstmt stmt_init) ^ " " ^ 
              string_of_opt_sexpr e_cond ^ "; " ^ 
              string_of_opt_sexpr e_trans ^ ")" ^ 
    string_of_sstmt stmt_l
  | SWhile (e, s)   -> "while (" ^ string_of_sexpr e ^ ") \n" ^ string_of_sstmt s
  | SVDecl (t, id, opt_expr) -> 
    string_of_typ t ^ " " ^ id ^
    (match opt_expr with None -> "" | Some(opt) -> " = " ^ string_of_sexpr opt) ^ ";\n"
  | SDelete(s)     -> "delete " ^ string_of_sexpr s ^ ";\n"
  | SFDef(f)       -> string_of_sfdef f
  | SReturn(e)     -> "return " ^ string_of_sexpr e ^ ";\n"
  | SBreak         -> "break;\n"
  | SContinue      -> "continue;\n"

and string_of_opt_sexpr = function
    None -> ""
  | Some sexpr -> string_of_sexpr sexpr

and string_of_opt_sstmt = function
    None -> "; "
  | Some(s) -> string_of_sstmt s

and string_of_sstmt_list l =
  String.concat "" (List.map string_of_sstmt l)

and string_of_sfdef fdef =
  string_of_typ fdef.srtyp ^ " " ^
  fdef.sfname ^ "(" ^ String.concat ", " (List.map snd fdef.sparams) ^ ")\n" ^ 
  string_of_sstmt(SBlock(fdef.sbody))

let string_of_sprogram (sstruct_decls, sstmts) =
  "\n\nSemantically checked program: \n\n" ^
  string_of_struct_decl_list sstruct_decls ^ string_of_sstmt_list sstmts
(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SBoolLit of bool
  | SStrLit of string
  | SArrayLit of sexpr list
  | SId of string
  | SUnaop of unaop * sexpr
  | SBinop of sexpr * binop * sexpr
  | SAssign of string * sexpr
  (* call *)
  | SCall of string * sexpr list

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  (* | SIf of sexpr * sstmt * sstmt *)
  | SWhile of sexpr * sstmt
  (* return *)
  | SReturn of sexpr

(* func_def: ret_typ fname formals locals body *)
type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sparams: bind list;
  (* slocals: bind list; *)
  sbody: sstmt list;
}

type sprogram = bind list * sfunc_def list (* TODO: sprogram is not in correspondence right now *)



(* Pretty-printing functions *)
let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
        SLiteral(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
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
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  (* | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2 *)
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sparams) ^
  ")\n{\n" ^
  (* String.concat "" (List.map string_of_vdecl fdecl.slocals) ^ *)
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)

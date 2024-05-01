(* Abstract Syntax Tree and functions for printing it *)

type unaop = Not
type binop = Mul | Div | Mod | Add | Sub | Equal | Neq | Ge | Le | Gt | Lt | And | Or

type typ = Int | Char | Bool | Float | String | Array of typ | Void

type expr =
  Noexpr
  | Literal of int
  | CharLit of char
  | BoolLit of bool
  | FloatLit of float
  | Id of string (* These are constructors; We can use Id(some_string) to construction a expr *)
  | StrLit of string
  | ArrayLit of expr list
  | Unaop of unaop * expr
  | Binop of expr * binop * expr
  | Assign of string * expr
  (* function call *)
  | Call of string * expr list

(* int x: name binding *)
type bind = typ * string

(* int x; is a bind or a expr (VDecl), but int x = 1; is a statement. *)
type stmt =
    Block of stmt list
  | Expr of expr
  (* if ... elif ... else ... *)
  | If of (expr * stmt) list * stmt
  (* for loop *)
  | For of (stmt option) * (expr option) * (expr option) * stmt list (* break is left for semantic checker? *)
  (* while: TODO: make expr optional *)
  | While of expr * stmt
  (* int a; or int a = 1 + 2; the expression is optional *)
  | VDecl of typ * string * expr option
  | SDef of string * (typ * string) list 
  (* return *)
  (* TODO: support return; *)
  | Return of expr
  | FDef of func_def (* Not first class function *)

(* func_def: ret_typ fname formals locals body *)
(* Mutually recursive data types with stmt: https://v2.ocaml.org/learn/tutorials/data_types_and_matching.html *)
and func_def = {
  rtyp: typ;
  fname: string;
  params: bind list;
  body: stmt list;
}

(* 
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}
*)

(* type program = bind list * func_def list *)
type program = stmt list

(* Pretty-printing functions *)
let string_of_unaop = function
    Not -> "!"
let string_of_binop = function
    Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Add -> "+"
  | Sub -> "-"
  | Ge -> ">="
  | Le -> "<="
  | Gt -> ">"
  | Lt -> "<"
  | Equal -> "=="
  | Neq -> "!="
  | And -> "&&"
  | Or -> "||"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | CharLit(c) -> Char.escaped c
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | FloatLit(f) -> string_of_float f
  | StrLit(s) -> String.escaped s
  | ArrayLit(a) -> 
    let rec string_of_list a = match a with
      [] -> ""
      | [element] -> string_of_expr element
      | hd::tl -> (string_of_expr (hd)) ^ "," ^ (string_of_list (tl)) 
    in "[" ^ string_of_list a ^ "]"
  | Id(s) -> s
  | Unaop(o, e) ->
    string_of_unaop o ^ string_of_expr e
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

(* let rec string_of_expr_option = function
  | None -> "None" (* Or empty *)
  | Some expr -> string_of_expr expr

let rec string_of_stmt_option = function
  | None -> "None" (* Or empty *)
  | Some stmt ->  string_of_stmt stmt *)

let rec string_of_typ = function
  Int -> "int"
| Char -> "char"
| Bool -> "bool"
| String -> "String"
| Array(t) -> "Array<" ^ string_of_typ t ^ ">"
| Float -> "float"
| Void -> ""

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"
let string_of_vdecl_list l = 
  let vdecls = List.map string_of_vdecl l in
  String.concat "" vdecls

(* Here, string_of_stmt, string_of_stmt_list, ..., string_of_fdef are all mutually recursive *)
let rec string_of_stmt = function
    Block(stmts) ->
    "\n" ^ "{" ^ string_of_stmt_list stmts ^ "}"
  | Expr(expr) -> string_of_expr expr ^ ";"
  | Return(expr) -> "return " ^ string_of_expr expr ^ "; "
  | If(e_s_l,Expr(Noexpr)) -> let string_of_if ((e, s)) =
    "if (" ^ string_of_expr e ^ ")\n" ^ (string_of_stmt s)
    in String.concat ("el") (List.map string_of_if (List.rev e_s_l))
  | If(e_s_l, s) ->
    let string_of_if ((e, s)) =
    "if (" ^ string_of_expr e ^ ")\n" ^ (string_of_stmt s)
    in String.concat (" " ^ "el") (List.map string_of_if (List.rev e_s_l)) ^
    (" ") ^ "else\n" ^ (string_of_stmt s)
  | For(stmt_init, e_cond, e_trans, stmt_l) ->
    "for (" ^ string_of_opt_stmt stmt_init ^ string_of_opt_expr e_cond ^ "; " ^
    string_of_opt_expr e_trans ^ ") {\n" ^ string_of_stmt_list stmt_l ^ "\n}"
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | VDecl (t, id, opt_expr) ->  (string_of_typ t) ^ " " ^ id ^ (match opt_expr with
    None -> "; " | Some(opt) -> " = " ^ string_of_expr opt ^ "; ")
  | SDef(s, l) -> "struct " ^ s ^ " {\n" ^ string_of_vdecl_list l  ^ "}\n"
  | FDef(f) -> string_of_fdef f


and string_of_stmt_list l =
  let stmts = List.map string_of_stmt l in
  String.concat "\n" stmts

and string_of_opt_stmt_list = function
  None -> ""
  | Some(l) -> string_of_stmt_list l

and string_of_opt_expr = function (* for an optional expr *)
  None -> ""
  | Some(e) -> string_of_expr e

and string_of_opt_stmt = function (* for an optional statement *)
  None -> ";"
  | Some(s) -> string_of_stmt s

and string_of_fdef fdef =
  string_of_typ fdef.rtyp ^ " " ^
  fdef.fname ^ "(" ^ String.concat ", " (List.map snd fdef.params) ^
  ")\n{\n" ^ 
   (string_of_stmt_list fdef.body) ^
  "\n}\n" 

let string_of_program (stmts_l) =
  "\n\nParsed program: \n\n" ^
  string_of_stmt_list stmts_l

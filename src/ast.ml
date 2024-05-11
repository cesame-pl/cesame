(* Abstract Syntax Tree and functions for printing it *)

type unaop = Not
type binop = Add | Sub | Mul | Div | Mod | Equal | Neq | Ge | Le | Gt | Lt | And | Or

type typ = Void | Char | Bool | Int | Float | String
  | Array of typ
  | Struct of string (* for example struct "Shape" *)
  | Func of typ list * typ

(* int x: name binding *)
type bind = typ * string

type expr =
    Noexpr
  | CharLit of char
  | BoolLit of bool
  | Literal of int
  | FloatLit of float
  | StrLit of string
  (* For struct object, Student a = new Student {name = "abc", age = 10}, not yet supported *)
  | StructLit of (string * expr) list
  | New of newable (* New(ArrayLit(...)) *)
  | AnonFunc of func_def (* Name is empty *)
  | Id of string (* These are constructors; We can use Id(some_string) to construction a expr *)
  | Unaop of unaop * expr
  | Binop of expr * binop * expr
  | Assign of expr * expr
  (* function call, myFunc(5, 3); *)
  | Call of string * expr list
  (* access member of a struct *)
  | AccessMember of expr * expr (* "a.name" "a[1].name" *)
  (* access element of an array *)
  | AccessEle of expr * expr (* "a[1]" "a[0][1]" *)

and newable =
  ArrayLit of expr list
  (* | NewFunc TODO *)

(* int x; is a bind or a expr (VDecl), but int x = 1; is a statement. *)
and stmt =
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
  (* newable *)
  | Delete of expr
  (* TODO: support return; *)
  | FDef of func_def (* Not first class function *)
  | Return of expr
  (* Do we want to add break and continue? Feels like it's going to complicate our control flow *)
  | Break
  | Continue

(* func_def: ret_typ fname params body *)
(* Mutually recursive data types with stmt: https://v2.ocaml.org/learn/tutorials/data_types_and_matching.html *)
and func_def = {
  rtyp: typ;
  fname: string;
  params: bind list;
  body: stmt list;
}

type struct_decl = {
  sname: string;
  body: bind list;
}

(* type program = struct_decl list option * stmt list *)
type program = struct_decl list * stmt list

(* Pretty-printing functions *)
let remove_last s =
  if String.length s > 0 then String.sub s 0 (String.length s - 1) else s

let string_of_unaop = function
    Not   -> "!"

let string_of_binop = function
    Add   -> "+"
  | Sub   -> "-"
  | Mul   -> "*"
  | Div   -> "/"
  | Mod   -> "%"
  | Equal -> "=="
  | Neq   -> "!="
  | Ge    -> ">="
  | Le    -> "<="
  | Gt    -> ">"
  | Lt    -> "<"
  | And   -> "&&"
  | Or    -> "||"

let rec string_of_typ = function
    Void        -> ""
  | Char        -> "char"
  | Bool        -> "bool"
  | Int         -> "int"
  | Float       -> "float"
  | String      -> "String"
  | Array (t)   -> "Array<" ^ string_of_typ t ^ ">"
  | Struct (s)  -> s
  | Func(tl, t) -> 
    "Func (" ^ String.concat ", " (List.map string_of_typ tl) ^ ") -> " ^ string_of_typ t

let string_of_bind (t, id) = 
  string_of_typ t ^ " " ^ id ^ ";\n"
let string_of_bind_list l = 
  String.concat "" (List.map string_of_bind l)

let string_of_struct_decl decl = 
  "struct " ^ decl.sname ^ "\n{\n" ^ string_of_bind_list decl.body  ^ "}\n"
let string_of_struct_decl_list l = 
  String.concat "" (List.map string_of_struct_decl l)

let rec string_of_expr = function
    Noexpr -> ""
  | CharLit (c) -> "'" ^ Char.escaped c ^ "'"
  | BoolLit (true) -> "true"
  | BoolLit (false) -> "false"
  | Literal (l) -> string_of_int l
  | FloatLit (f) -> string_of_float f
  | StrLit (s) -> "\"" ^ String.escaped s ^ "\""
  | StructLit (assign_list) ->
    "{ " ^ String.concat ", " (List.map string_of_dot_assign assign_list) ^ " }"
  | New(ArrayLit (a)) ->  
    let rec string_of_list a = match a with
        [] -> ""
      | [element] -> string_of_expr element
      | hd :: tl -> (string_of_expr hd) ^ ", " ^ (string_of_list tl) 
    in "[" ^ string_of_list a ^ "]"
  | AnonFunc (f) ->
    "(" ^ String.concat ", " (List.map snd f.params) ^ ") -> " ^ 
    (string_of_typ f.rtyp) ^ " " ^ remove_last (string_of_stmt(Block(f.body)))
  | Id (s) -> s
  | Unaop (o, e) -> string_of_unaop o ^ string_of_expr e
  | Binop (e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
  | Assign (e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Call (f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | AccessMember (e1, e2) -> string_of_expr e1 ^ "." ^ string_of_expr e2
  | AccessEle (e1, e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"

and string_of_dot_assign = function
  | (l, r) ->  "." ^ l ^ " = " ^ (string_of_expr r)

(* Here, string_of_stmt, string_of_stmt_list, ..., string_of_fdef are all mutually recursive *)
and string_of_stmt = function
    Block (stmts) -> "{\n" ^ string_of_stmt_list stmts ^ "}\n"
  | Expr (e)      -> string_of_expr e ^ ";\n"
  | If (l, s)     -> 
    let string_of_if (e, s) = "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s in 
      String.concat "el" (List.map string_of_if (List.rev l)) ^
      (match s with Expr(Noexpr) -> "" | _ -> "else\n" ^ string_of_stmt s)
  | For (stmt_init, e_cond, e_trans, stmt_l) ->
    "for (" ^ remove_last (string_of_opt_stmt stmt_init) ^ " " ^ 
              string_of_opt_expr e_cond ^ "; " ^ 
              string_of_opt_expr e_trans ^ ") " ^ 
    string_of_stmt(Block(stmt_l))
  | While (e, s) -> "while (" ^ string_of_expr e ^ ") \n" ^ string_of_stmt s
  | VDecl (t, id, opt_expr) ->
    string_of_typ t ^ " " ^ id ^ 
    (match opt_expr with None -> "" | Some(opt) -> " = " ^ string_of_expr opt) ^ ";\n"
  | Delete(s)   -> "delete " ^ string_of_expr s ^ ";\n"
  | FDef(f)     -> string_of_fdef f
  | Return(e)   -> "return " ^ string_of_expr e ^ ";\n"
  | Break       -> "break;\n"
  | Continue    -> "continue;\n"

and string_of_opt_expr = function (* for an optional expr *)
    None -> ""
  | Some(e) -> string_of_expr e

and string_of_opt_stmt = function (* for an optional statement *)
    None -> "; "
  | Some(s) -> string_of_stmt s

and string_of_stmt_list l =
  String.concat "" (List.map string_of_stmt l)

and string_of_fdef fdef =
  string_of_typ fdef.rtyp ^ " " ^
  fdef.fname ^ "(" ^ String.concat ", " (List.map snd fdef.params) ^ ")\n" ^ 
  string_of_stmt(Block(fdef.body))

let string_of_program (struct_decl_list, stmts) =
  "\n\nParsed program: \n\n" ^
  string_of_struct_decl_list struct_decl_list  ^  string_of_stmt_list stmts

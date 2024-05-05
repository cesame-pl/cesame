(* Abstract Syntax Tree and functions for printing it *)

type unaop = Not
type binop = Mul | Div | Mod | Add | Sub | Equal | Neq | Ge | Le | Gt | Lt | And | Or

type typ = Int | Char | Bool | Float | String | Array of typ | Void | Struct of string (* for example struct "Shape" *)

(* int x: name binding *)
type bind = typ * string

type expr =
  Noexpr
  | Literal of int
  | CharLit of char
  | BoolLit of bool
  | FloatLit of float
  | StrLit of string
  | ArrayLit of expr list
  | Id of string (* These are constructors; We can use Id(some_string) to construction a expr *)
  | Unaop of unaop * expr
  | Binop of expr * binop * expr
  | Assign of expr * expr
  (* function call, myFunc(5, 3); *)
  | Call of string * expr list
  | New of newable (* New(NewStruct(...)) *)
  (* access member of a struct *)
  | AccessMember of expr * expr (* "a.name" "a[1].name" *)
  (* access element of an array *)
  | AccessEle of expr * expr (* "a[1]" "a[0][1]" *)

(* "new Student" is an expression, "new Student {xxx} not yet supported " *)
and newable =
  NewStruct of string (* new struct object, "new Student {name = "abc", age = 10}" not yet supported, we're thinking whether to use expr list option or a new type to define the body *)
  (* | NewFunc TODO *)
(* For new struct object, Student a = new Student {name = "abc", age = 10}, not yet supported *)

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
  | SDef of string * bind list 
  | Delete of string
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

(* type program = bind list * func_def list *)
type program = stmt list

(* Pretty-printing functions *)
let remove_last s =
  if String.length s > 0 then String.sub s 0 (String.length s - 1) else s

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
  | CharLit(c) -> "'" ^ Char.escaped c ^ "'"
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | FloatLit(f) -> string_of_float f
  | StrLit(s) -> "\"" ^ String.escaped s ^ "\""
  | ArrayLit(a) ->  
    let rec string_of_list a = match a with
      [] -> ""
      | [element] -> string_of_expr element
      | hd::tl -> (string_of_expr (hd)) ^ ", " ^ (string_of_list (tl)) 
    in "[" ^ string_of_list a ^ "]"
  | Id(s) -> s
  | Unaop(o, e) -> string_of_unaop o ^ string_of_expr e
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
  | Assign(e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
  | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | New(n) -> string_of_newable n
  | AccessMember(e1, e2) -> string_of_expr e1 ^ "." ^ string_of_expr e2
  | AccessEle(e1, e2) -> string_of_expr e1 ^ "[" ^ string_of_expr e2 ^ "]"
  | Noexpr -> ""

and string_of_newable = function
  | NewStruct(s) -> "new " ^ s

and string_of_typ = function
  Int -> "int"
| Char -> "char"
| Bool -> "bool"
| String -> "String"
| Array(t) -> "Array<" ^ string_of_typ t ^ ">"
| Float -> "float"
| Struct(s) -> s;
| Void -> ""

let string_of_bind (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"
let string_of_bind_list l = 
  let vdecls = List.map string_of_bind l in
  String.concat "" vdecls

(* Here, string_of_stmt, string_of_stmt_list, ..., string_of_fdef are all mutually recursive *)
let rec string_of_stmt = function
    Block(stmts) -> "{\n" ^ string_of_stmt_list stmts ^ "}\n"
  | Expr(e)      -> string_of_expr e ^ ";\n"
  | If(l, s)     -> 
    let string_of_if (e, s) = "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s in 
      String.concat "el" (List.map string_of_if (List.rev l)) ^
      (match s with Expr(Noexpr) -> "" | _ -> "else\n" ^ string_of_stmt s)
  | For(stmt_init, e_cond, e_trans, stmt_l) ->
    "for (" ^ remove_last (string_of_opt_stmt stmt_init) ^ " " ^ 
              string_of_opt_expr e_cond ^ "; " ^ 
              string_of_opt_expr e_trans ^ ") " ^ 
    string_of_stmt(Block(stmt_l))
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") \n" ^ string_of_stmt s
  | VDecl (t, id, opt_expr) ->
    string_of_typ t ^ " " ^ id ^ 
    (match opt_expr with None -> "" | Some(opt) -> " = " ^ string_of_expr opt) ^ ";\n"
  | SDef(s, l)  -> "struct " ^ s ^ "\n{\n" ^ string_of_bind_list l  ^ "}\n"
  | Delete(s)   -> "delete " ^ s ^ ";\n"
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

let string_of_program (stmts_l) =
  "\n\nParsed program: \n\n" ^
  string_of_stmt_list stmts_l

(* Abstract Syntax Tree and functions for printing it *)

type unaop = Not
type binop = Mul | Div | Mod | Add | Sub | Equal | Neq | Ge | Le | Gt | Lt | And | Or

type typ = Int | Char | Bool | String | Array of typ | Void

type expr =
  Noexpr
  | Literal of int
  | CharLit of char
  | BoolLit of bool
  | Id of string
  | StrLit of string
  | ArrayLit of expr list
  | Unaop of unaop * expr
  | Binop of expr * binop * expr
  | Assign of string * expr
  (* function call *)
  | Call of string * expr list

type stmt =
    Block of stmt list
  | Expr of expr
  (* if ... elif ... else ... *)
  | If of (expr * stmt) list * stmt
  | While of expr * stmt
  (* return *)
  | Return of expr

(* int x: name binding *)
type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

(* type first_class_func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
} *)

type program = bind list * func_def list

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

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e_s_l,Expr(Noexpr)) -> let string_of_if ((e, s)) =
    "if (" ^ string_of_expr e ^ ")\n" ^ (string_of_stmt s)
    in String.concat ("el") (List.map string_of_if e_s_l)
  | If(e_s_l, s) ->
    let string_of_if ((e, s)) =
    "if (" ^ string_of_expr e ^ ")\n" ^ (string_of_stmt s)
    in String.concat (" " ^ "el") (List.map string_of_if e_s_l) ^
    (" ") ^ "else\n" ^ (string_of_stmt s)
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let rec string_of_typ = function
    Int -> "int"
  | Char -> "char"
  | Bool -> "bool"
  | String -> "String"
  | Array(t) -> "Array<" ^ string_of_typ t ^ ">"
  | Void -> ""

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

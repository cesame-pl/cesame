(* Semantic checking for the Cesame compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Helper function to create a struct map. *)
(* struct decl is a string name and a (typ * string) list *)

(* For struct, 2 main things to check: 
   Whether the object is of the correct type (the sname is the "type")
   Whether the object access correct member of correct type
*)

(* TODO: same idea to convert typ to LLtype *)
let rec checked_typ map t = match t with
| Struct str -> if StringMap.mem str map then t else raise (Failure ("struct type '" ^ str ^ "' not found"))
| Array ty -> ignore(checked_typ map ty); t
| _ -> t

(* let check_typ_list map typ_list =
  List.map (checked_typ map) typ_list *)

(* Takes in struct_decls_list returns a struct_decls_map *)
let make_struct_map struct_decls_list =
  let add_struct map s =
    if StringMap.mem s.sname map then
      raise (Failure("duplicate struct name '" ^ s.sname ^ "'"))
    else begin
      (* Check the s.body, which is a bind list. Make sure the typ is defined (For struct and possibly func) *)
      ignore(List.map (fun (ty, _) -> checked_typ map ty) s.body);
      StringMap.add s.sname (s.sname, s.body) map
    end
  in
  List.fold_left add_struct StringMap.empty struct_decls_list

let print_struct_map struct_map =
  StringMap.iter (fun name (n, binds) ->
    print_endline ("Struct name: " ^ n);
    print_endline "Fields:";
    print_endline ( String.concat ", " (List.map string_of_bind binds) );
    print_newline ();
  ) struct_map

(* TODO: Insert them into the func_decls_list *)
let buildin_funcs = [
  ("printint", {rtyp = Int; fname = "printint";
    params = [(Int, "x")]; body = []});
  ("print", {rtyp = Int; fname = "print";
    params = [(String, "s")]; body = []});
  ("println", {rtyp = Int; fname = "println";
    params = [(String, "s")]; body = []});
]

(* Insert them into the func_decls_list *)
let build_in_funcs = [
  { rtyp = Int; fname = "print"; params = []; body = [] };
  { rtyp = Int; fname = "println"; params = []; body = [] };
]

(* Helper function to create a function map *)
let make_func_map struct_map func_decls_list =
  let add_func map fd =
    match fd with
      _ when StringMap.mem fd.fname map ->
      if List.exists (fun tmp -> tmp.fname = fd.fname) build_in_funcs then 
        raise (Failure ("duplicate definition of built-in function " ^ fd.fname))
      else 
        raise (Failure ("duplicate function " ^ fd.fname))
    | _ -> StringMap.add fd.fname fd map in
    let built_in_decls = List.fold_left add_func StringMap.empty build_in_funcs in
  List.fold_left add_func built_in_decls func_decls_list

(* Helper function to find a function by name in the function map *)
let find_func fname func_map =
  try StringMap.find fname func_map
  with Not_found -> raise (Failure ("undeclared function '" ^ fname ^ "'"))

(* Helper function to create a symbol map for variables *)
(* TODO: Also check the struct map! To check if the Struct Name is there *)
let make_symbol_map struct_map bind_list =
  List.fold_left (fun m (ty, name) -> StringMap.add name (checked_typ struct_map ty) m) StringMap.empty bind_list

(* Helper function to find the type of a variable by its name *)
let type_of_identifier s symbols = 
  try StringMap.find s symbols
  with Not_found -> raise (Failure ("undeclared identifier " ^ s))

(* Helper function to check the assignment validity *)
(* Check assignment need information about the rhs expr, since it might be a struct literal. *)
let rec check_assign lvaluet rvaluet rexpr err =
  if lvaluet = rvaluet then lvaluet
  else match lvaluet with
    Array a -> ( (* a is the inner typ *)
        match rvaluet with
          Array Void -> lvaluet (* TODO: Why Void? *)
        | _ -> raise (Failure err)
      )
    | Struct str1 ->
      match rvaluet with
        Struct str2 -> if (str1 = str2) then lvaluet else raise (Failure err)
      (* TODO: More check for StructLit *)
      (* Void is for StructLit. Check every dot_assign's lhs is in the struct decl, and of the correct typ *)
      | Void -> Struct str1
      | _ -> raise (Failure err)
    | _ -> raise (Failure err)

(* Recursive function to check expressions *)
(* check_expr returns sexpr, which is typ * sx *)
let rec check_expr e struct_map bind_list func_decl_list = 
  let symbols = make_symbol_map struct_map bind_list in 
  let func_map = make_func_map struct_map func_decl_list in
  match e with
    Literal l -> (Int, SLiteral l)
  | Noexpr -> (Void, Noexpr)
  | CharLit c -> (Char, SCharLit c)
  | BoolLit l -> (Bool, SBoolLit l)
  | StrLit s  -> (String, SStrLit s)
  | FloatLit f -> (Float, SFloatLit f)
  | StructLit dot_assigns_list -> 
    (Void, SStructLit (
      List.map (fun (ty, ex)-> (ty, check_expr ex struct_map bind_list func_decl_list)) dot_assigns_list)
    ) (* StructLit's type can't be determined yet, so void *)
  | New(ArrayLit l) -> (* l is an expression list *)
    let rec check_array_helper l prev_typ =
      match l with
        [] -> prev_typ
      | [e] -> (
        if (prev_typ = fst (check_expr e struct_map bind_list func_decl_list)) then prev_typ
        else raise (Failure ("array type '" ^ (string_of_typ prev_typ) ^ "' inconsistent with type '" ^ (string_of_typ (fst (check_expr e struct_map bind_list func_decl_list)) ^ "'")))
      )
      | hd :: tl -> (
        if (prev_typ = fst (check_expr hd struct_map bind_list func_decl_list)) then (check_array_helper tl prev_typ)
        else raise (Failure ("array type '" ^ (string_of_typ prev_typ) ^ "' inconsistent with type '" ^ (string_of_typ (fst (check_expr hd struct_map bind_list func_decl_list))^ "'")))
      )
    in 
    let check_array l = 
      match l with 
        [] -> (Array(Void), SNew(SArrayLit []))
      | hd :: tl -> (Array(check_array_helper l (fst (check_expr hd struct_map bind_list func_decl_list))), SNew(SArrayLit (List.map (fun x -> check_expr x struct_map bind_list func_decl_list) l)))
    in 
    check_array l
  | Id var -> (type_of_identifier var symbols, SId var)
  | Unaop(op, e) ->
    (match op with 
      Not -> (Bool, SUnaop(op, check_bool_expr e struct_map bind_list func_decl_list))
    | Neg -> 
      let e' = check_expr e struct_map bind_list func_decl_list in 
      if (fst e') <> Int then raise (Failure "unary minus can only apply to integers")
      else (Int, SUnaop(op, e')))
  | Binop(e1, op, e2) as e ->
    let (t1, e1') = check_expr e1 struct_map bind_list func_decl_list
    and (t2, e2') = check_expr e2 struct_map bind_list func_decl_list in
    let err = "illegal binary operator '" ^
              string_of_typ t1 ^ " " ^ string_of_binop op ^ " " ^
              string_of_typ t2 ^ "' in '" ^ string_of_expr e ^ "'"
    in
    (* All binary operators require operands of the same type *)
    if t1 = t2 then
      (* Determine expression type based on operator and operand types *)
      let t = match op with
          Add | Sub | Mul | Div | Mod when t1 = Int -> Int
        | Equal | Neq -> Bool
        | Ge | Le | Lt | Gt when t1 = Int -> Bool
        | And | Or when t1 = Bool -> Bool
        | _ -> raise (Failure err)
      in
      (t, SBinop((t1, e1'), op, (t2, e2')))
    else raise (Failure err)
  | Assign(e1, e2) as ex ->
    (* lt: left type, rt: right type *)
    let (lt, e1) = check_expr e1 struct_map bind_list func_decl_list
    and (rt, e2) = check_expr e2 struct_map bind_list func_decl_list in
    let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex in
    (check_assign lt rt e2 err, SAssign((lt, e1), (rt, e2)))
  (* for print calls, skip parameter validation since the types and numbers of parameters are unknown. *)
  | (Call("print", args) as call)
  | (Call("println", args) as call) ->
    if List.length args != 1 then 
      raise (Failure ("expecting 1 argument in " ^ string_of_expr call))
    else 
      let name = (match call with Call("println", _) -> "println" | _ -> "print") in
      let args' = List.map (fun e -> check_expr e struct_map bind_list func_decl_list) args in 
      (Int, SCall(name, args'))
  (* check_assign here is pretty interesting, we're basically assigning expr value to the func args *)
  | Call(fname, args) as call ->
    let fd = find_func fname func_map in
    let param_length = List.length fd.params in
    if List.length args != param_length then
      raise (Failure ("expecting " ^ string_of_int param_length ^ " arguments in " ^ string_of_expr call))
    else 
      let check_call (ft, _) e =
        let (et, e') = check_expr e struct_map bind_list func_decl_list in
        let err = "illegal argument found " ^ string_of_typ et ^ " expecting " ^ string_of_typ ft ^ " in " ^ string_of_expr e in
        (check_assign ft et e' err, e') (* Here *)
    in
    let args' = List.map2 check_call fd.params args in
    (fd.rtyp, SCall(fname, args'))
  (*
    Check access_member is in 2 fold:
    First, e1 has to be evaulated to be an ID, whose name is in the struct map
    e2 has to be evaulated to be a struct, so
  *)
  (* Only supports single layer of access for now *)
  | AccessMember (e1, e2) -> 
    let rec find_bind bind_list str =
      match bind_list with
    | [] -> raise (Failure "not a field of the struct.")
    | (typ, s) :: tl ->
      if s = str then
        typ
      else
        find_bind tl str
    in
    let se1 = check_expr e1 struct_map bind_list func_decl_list in
    (* let se2 = check_expr e2 struct_map bind_list func_decl_list in *)
    (match se1, e2 with
    (Struct n, _), Id s ->
      try let (_, bl) =  StringMap.find n struct_map in (find_bind bl s, SAccessMember(se1, (find_bind bl s, SId s)))
        with Not_found -> raise (Failure("TODO"))
    | _ -> raise (Failure("TODO")))
  
  | AccessEle(e1, e2) -> (* TODO: out of bound *)
    let se1 = check_expr e1 struct_map bind_list func_decl_list in
    let se2 = check_expr e2 struct_map bind_list func_decl_list in 
    (match fst se1, fst se2 with 
      Array t, Int -> (t, SAccessEle(se1, se2))
    | Array _, _ -> raise (Failure "index must be an integer value.")
    | _ -> raise (Failure("cannot apply the operator '[]' to " ^ string_of_expr e1)))
  | _ -> raise (Failure("TODO:\n" ^ string_of_expr e))

(* Do not check type yet, only eval *)
(* TODO *)
(* and check_dot_assign (s, ex) = (s, check_expr ex ...)
and check_dot_assign_list assign_list = 
  match stmt with 
  [] -> []
  | s :: sl ->  *)

(* Function to check if an expression is boolean *)
and check_bool_expr e struct_map bind_list function_list = 
  let (t, e') = check_expr e struct_map bind_list function_list in
  match t with
    Bool -> (t, e')
  |  _ -> raise (Failure ("expecting Boolean expression in '" ^ string_of_expr e ^ "'"))

(* Function to check variable declarations *)
(* check the declarations, pass the local declaration and local functions, return the new declaration list, the original function list and the checked sast *)
let check_vdecl struct_map globals locals func_decl_list vdecl = 
  let symbols = make_symbol_map struct_map locals in
  (* Make a map according to the locals list and find variable name in it. If not found, then it is fine, or its a duplicate declaration and need to throw an error *)
  match vdecl with
    (t, s, None) -> 
    (match StringMap.find_opt s symbols with 
      None -> ((t,s)::locals, SVDecl(t, s, None))
    | _ -> raise (Failure("duplicate definition of " ^ s ^ "!\n")))
  | (t, s, Some e) -> 
    (match StringMap.find_opt s symbols with 
      None -> let (rt, ex) = check_expr e struct_map (globals @ locals) func_decl_list in (if t = rt then ((t,s)::locals, SVDecl(t, s, Some (rt,ex))) else raise(Failure(string_of_typ t ^ " does not match " ^ string_of_typ rt)))
    | _ -> raise (Failure("duplicate definition of " ^ s ^ "!\n")))

(* Function to check statement lists *)
(* Params: 
  - globals: list of variables in the parent block.
  - locals: list of variables to be added in this block.
  - global_func_decls & local_func_decls: lists similar to globals and locals, but for function definitions.
  - rtyp: return type of the function to which the block belongs, the program will by default returns an int.
  Returns: 
  - a list of sstmt *)
let rec check_stmt_list struct_map globals locals global_func_decls local_func_decls rtyp in_loop stmt = 
  match stmt with 
    [] -> []
  | s :: sl ->
    let (new_locals, new_local_func_decls, s_stmt) =
      check_stmt struct_map globals locals global_func_decls local_func_decls rtyp in_loop s in
    s_stmt :: check_stmt_list struct_map globals new_locals global_func_decls new_local_func_decls rtyp in_loop sl 

(* Function to check individual statements *)
(* Params: (same as check_stmt_list)
  Returns: 
    - new locals
    - new local func decls
    - semantically validated sstmt
    (globals are not required since the statement does not modify global variables.) *)
and check_stmt struct_map globals locals global_func_decls local_func_decls rtyp in_loop stmt =
  (* vars: all variables; func_decls: all functions declarations *)
  let vars = globals @ locals and func_decls = global_func_decls @ local_func_decls in 
  
  match stmt with
  (* A Block is valid if 
    - every stmt within it is correct
    - nothing follows a return stmt *)
    Block b -> 
    let sstmts = check_stmt_list struct_map vars [] func_decls [] rtyp in_loop b in
    (locals, local_func_decls, SBlock(sstmts))
  | Expr e -> 
    let sexpr = check_expr e struct_map vars func_decls in 
    (locals, local_func_decls, SExpr(sexpr))
  (* An If is valid if
    - bool expression in expr list
    - stmts are valid *)
  | If(l, s) -> 
    let get_sstmt stmt = let (_, _, sstmt) = check_stmt struct_map vars [] func_decls [] rtyp in_loop stmt in sstmt in
    let check_if (le, ls) = (check_bool_expr le struct_map vars func_decls, get_sstmt ls) in
    (locals, local_func_decls, SIf(List.map check_if l, get_sstmt s))
  (* A For is valid if 
    - the initial statement is a single statement (do not support block, if, while, etc) 
    - check end condition (if any) is a vaild bool condition
    - check the trans expression
    - check the statement list *)
  | For(stmt_init, e_cond, e_trans, stmt_l) ->
    let (new_locals, _, s_init_stmt) = 
      match stmt_init with 
        Some s -> 
        (match s with
          Expr _ | VDecl(_, _, _) -> 
          let (nl, nf, ssit) = check_stmt struct_map vars [] func_decls [] rtyp true s in
          (nl, nf, Some ssit)
        | _ -> raise (Failure("for-loop only supports single-line statements"))) 
      | None -> (locals, local_func_decls, None) 
    in
    let s_end_cond = 
      match e_cond with 
        Some ec -> Some (check_bool_expr ec struct_map (vars @ new_locals) func_decls)
      | None -> None 
    in 
    let s_trans_e = 
      match e_trans with 
        Some ec -> Some (check_expr ec struct_map (vars @ new_locals) func_decls)
      | None -> None 
    in 
    let (_, _, ss_l) = 
      check_stmt struct_map (vars @ new_locals) [] func_decls [] rtyp true (Block(stmt_l))in 
    (locals, local_func_decls, SFor(s_init_stmt, s_end_cond, s_trans_e, ss_l))
  | While (e, s) -> 
    let (_, _, sstmts) = check_stmt struct_map globals locals global_func_decls local_func_decls rtyp true s in 
    let swhile = SWhile(check_bool_expr e struct_map vars func_decls, sstmts) in 
    (locals, local_func_decls, swhile)
  (* A VDecl is valid if 
    - no duplicate local var decl *)
  | VDecl (t, s, eop) -> 
    let (new_locals, svdec) = check_vdecl struct_map globals locals func_decls (t, s, eop) in 
    (new_locals, local_func_decls, svdec)
  (* A FDef is valid if 
    - no duplicate function name (check_fname)
    - no duplicate params (check_param)
    - body stmt list is valid *)
  | Delete (e) -> 
    let se = check_expr e struct_map vars func_decls in
    (match se with 
      (Array _, _) -> (locals, func_decls, SDelete se)
    | _ -> raise (Failure ("cannot delete this data type '" ^ (string_of_typ (fst se)) ^ "'")))
  | FDef (f) ->
    (* check whether there are duplicate params *)
    let check_param bind_list p =
      let p_map = make_symbol_map struct_map bind_list in
      match StringMap.find_opt (snd p) p_map with
        None -> p :: bind_list
      | Some n -> raise (Failure("duplicate bind of '" ^ (snd p) ^ "' in function '" ^ f.fname ^ "'"))
    in
    let rec check_params bind_list pl =
      match pl with
        [] -> []
      | p :: pl -> let new_bind_list = check_param bind_list p in p :: check_params new_bind_list pl
    in
    let sbinds = check_params [] f.params in
    (* check whether there are dupilcate function names *)
    let check_fname name =
      let f_map = make_func_map struct_map (global_func_decls @ local_func_decls) in
      match StringMap.find_opt name f_map with
        None -> name 
      | Some f -> raise (Failure("duplicate function name '" ^ name ^ "'"))
    in
    let sname = check_fname f.fname in
    (* check the statements
      Inputs:
      - globals: globals @ locals
      - locals: params
      - global_func_decls: global_func_decls @ local_func_decls
      - local_func_decls: <itself> *)
    let sstmts = check_stmt_list struct_map vars sbinds func_decls [f] f.rtyp false f.body in
    let sfdef = SFDef({srtyp = f.rtyp; sfname = sname; sparams = f.params; sbody = sstmts}) in
    (locals, f::local_func_decls, sfdef)
  | Return e ->
    let (t, e') = check_expr e struct_map vars func_decls in
    if t = rtyp then
      (locals, local_func_decls, SReturn (t, e'))
    else
      raise (Failure ("return gives " ^ string_of_typ t ^ " expecting " ^ string_of_typ rtyp ^ " in " ^ string_of_expr e))
  | Break -> if in_loop then (locals, local_func_decls, SBreak) else raise(Failure("The break statement is not in a loop"))
  | Continue -> if in_loop then (locals, local_func_decls, SContinue) else raise(Failure("The continue statement is not in a loop"))

(* Entry point for the semantic checker *)
let check (struct_decls, stmts) = 
  (* check struct_decls *)
  let struct_map = make_struct_map struct_decls in
  (* let _ = print_struct_map struct_map in *)
  (struct_decls, (* TODO: return sstruct_decls *)
  (* check statements *)
  check_stmt_list struct_map [] [] [] [] Int false stmts)

(* Semantic checking for the Cesame compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check(stmts) = 
let make_func_map func_decls_list = 
  let built_in_decls = StringMap.add "print" {
    rtyp = Int;
    fname = "print";
    params = [(Int, "x")];
    (* TODO: locals *) body = [] } StringMap.empty in
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map in
  List.fold_left add_func built_in_decls func_decls_list in 
let find_func fname func_map = 
  try StringMap.find fname func_map
  with Not_found -> raise (Failure ("unrecognized function " ^ fname)) in 

let make_symbol_map bind_list = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
        StringMap.empty bind_list in
let type_of_identifier s symbols = 
  (try StringMap.find s symbols
  with Not_found -> raise (Failure ("undeclared identifier " ^ s))) in
let check_assign lvaluet rvaluet err = 
  if lvaluet = rvaluet then lvaluet 
  else match lvaluet with Array(a) -> (
      match rvaluet with Array(Void) -> lvaluet
      | _ -> raise (Failure err)
  )
  | _ ->  raise (Failure err) in 
let rec check_expr e bind_list func_decl_list = 
  let symbols = make_symbol_map(bind_list) in 
  let func_map = make_func_map(func_decl_list) in
  match e with
   Literal l -> (Int, SLiteral l)
  | Noexpr -> (Void, Noexpr)
  | BoolLit l -> (Bool, SBoolLit l)
  | StrLit s  -> (String, SStrLit s)
  | ArrayLit l -> let rec check_array_helper l prev_typ = (
    match l with
      [] -> prev_typ
    | [element] -> (
      if (prev_typ = fst (check_expr element bind_list func_decl_list)) then prev_typ
      else raise (Failure ("Array type " ^ (string_of_typ prev_typ) ^ "inconsistent with type " ^ (string_of_typ (fst (check_expr element bind_list func_decl_list)))))
    )
    | hd::tl -> (
        if (prev_typ = fst (check_expr hd bind_list func_decl_list)) then (check_array_helper tl prev_typ)else raise (Failure ("Array type " ^ (string_of_typ prev_typ) ^ "inconsistent with type " ^ (string_of_typ (fst (check_expr hd bind_list func_decl_list)))))
    )
  ) in let check_array l = (
    match l with 
      [] -> (Array(Void), SArrayLit [])
      | hd::tl -> (Array(check_array_helper l (fst (check_expr hd bind_list func_decl_list))), SArrayLit (List.map (function x -> check_expr x bind_list func_decl_list) l))
  ) in check_array l
  | Id var -> (type_of_identifier var symbols, SId var)
  (* TODO: var now is an expr instead of str *)
  | Assign(e1, e2) as ex ->
    (* lt: left type, rt: right type *)
    let (lt, e1) = check_expr e1 bind_list func_decl_list
    and (rt, e2) = check_expr e2 bind_list func_decl_list in
    let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
              string_of_typ rt ^ " in " ^ string_of_expr ex
    in
    (check_assign lt rt err, SAssign((lt, e1), (rt, e2)))

  | Binop(e1, op, e2) as e ->
    let (t1, e1') = check_expr e1 bind_list func_decl_list
    and (t2, e2') = check_expr e2 bind_list func_decl_list in
    let err = "illegal binary operator " ^
              string_of_typ t1 ^ " " ^ string_of_binop op ^ " " ^
              string_of_typ t2 ^ " in " ^ string_of_expr e
    in
    (* All binary operators require operands of the same type*)
    if t1 = t2 then
      (* Determine expression type based on operator and operand types *)
      let t = match op with
          Add | Sub when t1 = Int -> Int
        | Equal | Neq -> Bool
        | Lt when t1 = Int -> Bool
        | Gt when t1 = Int -> Bool
        | And | Or when t1 = Bool -> Bool
        | _ -> raise (Failure err)
      in
      (t, SBinop((t1, e1'), op, (t2, e2')))
    else raise (Failure err)
  | Call(fname, args) as call ->
    let fd = find_func fname func_map in
    let param_length = List.length fd.params in
    if List.length args != param_length then
      raise (Failure ("expecting " ^ string_of_int param_length ^
                      " arguments in " ^ string_of_expr call))
    else let check_call (ft, _) e =
           let (et, e') = check_expr e bind_list func_decl_list in
           let err = "illegal argument found " ^ string_of_typ et ^
                     " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
           in (check_assign ft et err, e')
      in
      let args' = List.map2 check_call fd.params args
      in (fd.rtyp, SCall(fname, args'))  in 
let check_bool_expr e bind_list function_list = 
  let (t, e') = check_expr e bind_list function_list in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e)) in
(*check the declarations, pass the local declaration and local functions, return the new declaration list, the original function list and the checked sast*)
let check_vdecl bind_list func_decl_list vdecl = 
  let symbols = make_symbol_map(bind_list) in
  (*Make a map according to the locals list and find variable name in it. If not found, then it is fine, or its a depilcated declaration and need to throw an errot*)
  match vdecl with
    (t, s, None) -> (match StringMap.find_opt s symbols with 
                      None -> ((t,s)::bind_list, SVDecl(t, s, None))
                      | _ -> raise(Failure("Duplicated definition of "^s^"!\n")))
    |(t, s, Some e) -> (match StringMap.find_opt s symbols with 
                  None -> let (rt, ex) = check_expr e bind_list func_decl_list in (if t = rt then ((t,s)::bind_list, SVDecl(t, s, Some (rt,ex))) else raise(Failure(string_of_typ t ^ " does not match " ^ string_of_typ rt)))

                  | _ -> raise(Failure("Duplicated definition of "^s^"!\n"))) in 
  
let rec check_stmt_list globals locals global_func_decls local_func_decls rtyp s = 
match s with
[] -> []
| s :: sl -> let (new_locals, new_local_func_decls, s_stmt) = check_stmt globals locals global_func_decls local_func_decls rtyp s in s_stmt :: check_stmt_list globals new_locals global_func_decls new_local_func_decls rtyp sl
(* Return a semantically-checked statement i.e. containing sexprs *)
and check_stmt globals locals global_func_decls local_func_decls rtyp =function
(* A block is correct if each statement is correct and nothing
 follows any Return statement.  Nested blocks are flattened. *)
Block sl -> (locals, local_func_decls, SBlock (check_stmt_list (globals @ locals) [] (global_func_decls @ local_func_decls) [] rtyp sl))
| Expr e -> (locals, local_func_decls, SExpr ((check_expr e (globals @ locals) (global_func_decls @ local_func_decls))))
(* | If(e, st1, st2) ->
SIf(check_bool_expr e, check_stmt st1, check_stmt st2) *)

| VDecl(t, s, eop) -> let (new_locals, svdec) = check_vdecl locals (global_func_decls @ local_func_decls) (t, s, eop) in (new_locals, local_func_decls, svdec)
| While(e, st) ->
(locals, local_func_decls, let (_, _, sstmts) = check_stmt (globals @ locals) [] (global_func_decls @ local_func_decls) [] rtyp st in SWhile(check_bool_expr e (globals @ locals) (global_func_decls @ local_func_decls), sstmts))
| Return e ->
let (t, e') = check_expr e (globals @ locals) (global_func_decls @ local_func_decls) in
if t = rtyp then (locals, local_func_decls, SReturn (t, e'))
else raise (
    Failure ("return gives " ^ string_of_typ t ^ " expected " ^
             string_of_typ rtyp ^ " in " ^ string_of_expr e))
| FDef(f) -> let check_func globals locals global_func_decls local_func_decls f =
  (*Check whether there is deplicated params*)
  let check_param bind_list p = let p_map = make_symbol_map bind_list in match StringMap.find_opt (snd p) p_map with
    None -> p::bind_list
    | Some n -> raise(Failure("Duplicated bind of " ^ (snd p) ^ " in function " ^ f.fname )) in 
  let rec check_params bind_list pl = (match pl with
     [] -> []
    | p::pl -> (let new_bind_list = check_param bind_list p in p::check_params new_bind_list pl))
  in let sbinds = check_params [] f.params 
  (*Check whether there is dupilcated function name*)
in let check_fname name = let f_map = make_func_map (global_func_decls @ local_func_decls) in match StringMap.find_opt name f_map with 
  Some f -> raise(Failure("Do not support duplicated function name " ^ name))
  | None -> name in let sname = check_fname f.fname in 
  (*Check the statements, with the globals being a joint list of current locals and globals, with the same for functions. Local variables are parameters, local function is only itself and the return type is the return type of the funtion*)
  let sstmts = check_stmt_list (globals @ locals) sbinds (global_func_decls @ local_func_decls) [f] f.rtyp f.body in (locals, f::local_func_decls, SFDef({srtyp = f.rtyp; sfname = sname; sparams = f.params; sbody = sstmts})) in check_func globals locals global_func_decls local_func_decls f
| If(l, s) -> let check_if = 
  function (le, ls) -> (check_bool_expr le (globals @ locals) (global_func_decls @ local_func_decls) , let (_, _, lss) = (check_stmt (globals @ locals) [] (global_func_decls @ local_func_decls) [] rtyp ls) in lss) in 
  let sl = List.map check_if l in let (_, _, ss) = check_stmt (globals @ locals) [] (global_func_decls @ local_func_decls) [] rtyp s in (locals, local_func_decls, SIf(sl, ss))
| For(init_stmt, end_cond, trans_e, s_l) -> let check_single_stmt gl ll gfl lfl rtyp s = 
  match s with 
    Expr e -> check_stmt gl ll gfl lfl rtyp s 
  | VDecl(t, symbol, eop) -> check_stmt gl ll gfl lfl rtyp s
  | _ -> raise(Failure("For loop only support single line statement")) in
let (new_locals, _, s_init_stmt) = 
(*return a new locals list to make sure that the expressions afterwards can see the init_stmt in case init statement is a definition*)
(match init_stmt with 
Some sit -> let (nl, nf, ssit) = check_single_stmt (globals @ locals) [] (global_func_decls @ local_func_decls) [] rtyp sit in (nl, nf, Some ssit)
| None -> (locals, local_func_decls, None)) in 
let s_end_cond = match end_cond with 
  Some ec -> Some (check_bool_expr ec (globals @ locals @ new_locals) (global_func_decls @ local_func_decls))
| None -> None in 
let s_trans_e = (match trans_e with 
Some ec -> Some (check_expr ec (globals @ locals @ new_locals) (global_func_decls @ local_func_decls))
| None -> None) in 
let (_, _, ss_l) = check_stmt (globals @ locals @ new_locals) [] (global_func_decls @ local_func_decls) [] rtyp (Block(s_l))
in (locals, local_func_decls, SFor(s_init_stmt, s_end_cond, s_trans_e, ss_l))
in check_stmt_list [] [] [] [] Int stmts
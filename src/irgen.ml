module L = Llvm
module A = Ast
open Sast 
open Ast

module StringMap = Map.Make(String)

(* C Array v.s. Java Array; C char* v.s. Java String *)

(* When we enter a new scope, we combine the globals and locals to pass in as the globals of the new scope. *)
(* iterate over locals and insert its key-value pairs into the globals, to return *)
let combine_maps globals locals =
  let combined_map = ref globals in
  StringMap.iter (fun key value ->
    combined_map := StringMap.add key value !combined_map
  ) locals;
  !combined_map

let translate (program: sstmt list) : Llvm.llmodule = 
  
  (* llvm envs *)
  let context = L.global_context () in
  let the_module = L.create_module context "cesame" in

  (* llvm types *)
  let i32_t      = L.i32_type       context (* integer *)
  and f64_t      = L.double_type    context (* float   *)
  and char_t       = L.i8_type      context (* Char    *)
  and char_pt    = L.pointer_type   (L.i8_type context) (* Char* *)
  and bool_t        = L.i1_type     context (* Bool    *)

  in

  (* return llvm type for sast type *)
  let ltype_of_typ = function
    | A.Int -> i32_t
    | A.Float -> f64_t
    | A.Bool -> bool_t
    | A.Char -> char_t
  in
  
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
    | Some _ -> ()
    | None -> ignore (instr builder) 
  in

  let int_format_str builder = L.build_global_stringptr "%d\n" "fmt" builder
  and float_format_str builder = L.build_global_stringptr "%g\n" "fmt" builder
  in

  let printf_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type char_t |] in
  let printf_func : L.llvalue = 
    L.declare_function "printf" printf_t the_module in

  let printbig_t : L.lltype =
    L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
    L.declare_function "printbig" printbig_t the_module in

  (* Helper function to look up variable from both the global and the local list of variable (local first) *)
  let var_addr_lookup globals locals var_name: L.llvalue = 
    try StringMap.find var_name locals
      with Not_found -> StringMap.find var_name globals
  in

  (* Takes in an sexpr *)
  let rec build_expr globals locals builder ((_, e) : sexpr) : L.llvalue = 
    match e with
     SLiteral(i)         -> L.const_int i32_t i
    | SFloatLit(f)        -> L.const_float f64_t f
    | SBoolLit (b)  -> L.const_int bool_t (if b then 1 else 0)
    | SCharLit (c) -> L.const_int (ltype_of_typ Char) (Char.code c)
    | SId (s) -> L.build_load (var_addr_lookup globals locals s) s builder
    | SAssign ((lt, se1), (rt, se2)) -> 
      let e' = build_expr globals locals builder (rt, se2) in
      let s = match se1 with
        SId var -> var
        | _ -> raise (Failure("LHS cannot be assigned to."))
      in ignore(L.build_store e' (var_addr_lookup globals locals s) builder); e'
    | SCall ("print", [e]) 
    | SCall ("printb", [e]) ->
	    L.build_call printf_func [| int_format_str builder; (build_expr globals locals builder e) |]
	    "printf" builder
    | SCall ("printbig", [e]) ->
	    L.build_call printbig_func [| (build_expr globals locals builder e) |] "printbig" builder
    | SCall ("printf", [e]) -> 
	    L.build_call printf_func [| float_format_str builder; (build_expr globals locals builder e) |]
	    "printf" builder
    | SUnaop (op, e) ->
      let e' = build_expr globals locals builder e in 
      (match op with 
      A.Not -> L.build_not e' "tmp" builder)
    | SBinop (e1, op, e2) ->
      (* check if e1 and e2 are valid in semant *)
      (* print_string (string_of_sexpr((t,  SBinop(e1, op, e2)))); *)
      let e1' = build_expr globals locals builder e1 in
      let e2' = build_expr globals locals builder e2 in
      let err = "Unsupported binary operation for this type" in
      let (e1_t, _) = e1 in
      (match e1_t with
      | A.Bool -> 
        (match op with 
        A.And -> L.build_and
        | A.Or -> L.build_or
        | A.Equal -> L.build_icmp L.Icmp.Eq
        | A.Neq -> L.build_icmp L.Icmp.Ne
        | A.Ge -> L.build_icmp L.Icmp.Sge
        | A.Le -> L.build_icmp L.Icmp.Sle
        | A.Gt -> L.build_icmp L.Icmp.Sgt
        | A.Lt -> L.build_icmp L.Icmp.Slt)
      | A.Int ->
        (match op with 
        A.And -> L.build_and
        | A.Or -> L.build_or
        | A.Add -> L.build_add
        | A.Sub -> L.build_sub 
        | A.Mul -> L.build_mul
        | A.Div -> L.build_sdiv (* signed division *)
        | A.Mod -> L.build_srem (* signed remainder *)
        | A.Equal -> L.build_icmp L.Icmp.Eq
        | A.Neq -> L.build_icmp L.Icmp.Ne
        | A.Ge -> L.build_icmp L.Icmp.Sge
        | A.Le -> L.build_icmp L.Icmp.Sle
        | A.Gt -> L.build_icmp L.Icmp.Sgt
        | A.Lt -> L.build_icmp L.Icmp.Slt)
      | A.Float -> 
        (match op with
        A.And -> L.build_and
        | A.Or -> L.build_or
        | A.Add -> L.build_fadd
        | A.Sub -> L.build_fsub 
        | A.Mul -> L.build_fmul
        | A.Div -> L.build_fdiv
        | A.Equal -> L.build_fcmp L.Fcmp.Oeq
        | A.Neq -> L.build_fcmp L.Fcmp.One
        | A.Ge -> L.build_fcmp L.Fcmp.Oge
        | A.Le -> L.build_fcmp L.Fcmp.Ole
        | A.Gt -> L.build_fcmp L.Fcmp.Ogt
        | A.Lt -> L.build_fcmp L.Fcmp.Olt) 
      |_ -> raise (Failure err)) e1' e2' "tmp" builder
    | Noexpr -> L.const_int i32_t 0
  in
  let rec build_stmt globals locals builder lfunc= function
    SExpr(se) -> ignore(build_expr globals locals builder se); (locals, builder)
    (* If it's a declaration, insert it into the locals. The bind is the first element of locals *)
    (* Why do we need local_func_decls here? *)
    (* TODO: If t is a Func type? *)
    | SVDecl (t, var_name, None) -> 
      let lhs_addr = match t with
        (* TODO: ADD functions *)
        | _ -> L.build_alloca (ltype_of_typ t) var_name builder
      in
      let new_locals = StringMap.add var_name lhs_addr locals (* Add var_name -> var_addr to the map *)
      in (new_locals, builder)
    | SVDecl (t, var_name, Some (rt, re)) -> (* right type and right expression *)
      let lhs_addr = match t with
        (* TODO: ADD functions *)
        | _ -> L.build_alloca (ltype_of_typ t) var_name builder
      in let new_locals = StringMap.add var_name lhs_addr locals (* Add var_name -> var_addr to the map *)
      in let e' = build_expr globals locals builder (rt, re)
      in ignore(L.build_store e' lhs_addr builder);
      (new_locals, builder)
    | SBlock(sstmt_l) -> let new_builder = build_stmt_list (combine_maps globals locals) StringMap.empty builder lfunc sstmt_l in  (locals, new_builder)
    | SIf(se_sst_l, sst) -> let rec build_if globals locals builder = 
      function
      ([] , sst) -> let (_, new_builder) = build_stmt globals locals builder lfunc sst in new_builder 
      | ((fse, then_sst)::else_sl, sst) -> (let bool_val = build_expr globals locals builder fse in 
      let merge_bb = L.append_block context "merge" lfunc in 
      let b_br_merge = L.build_br merge_bb in 
      let then_bb = L.append_block context "then" lfunc in 
      let (_, then_stmt_builder) = build_stmt globals locals (L.builder_at_end context then_bb) lfunc then_sst in 
      add_terminal then_stmt_builder b_br_merge; 
      let else_bb = L.append_block context "else" lfunc in 
      let else_stmt_builer = build_if globals locals (L.builder_at_end context else_bb) (else_sl, sst) in 
      add_terminal else_stmt_builer b_br_merge;
      ignore(L.build_cond_br bool_val then_bb else_bb builder);
      L.builder_at_end context merge_bb) in
      (locals, build_if globals locals builder (se_sst_l, sst))
    | SWhile(se, sst) -> let pred_bb = L.append_block context "while" lfunc in 
    ignore(L.build_br pred_bb builder);
    let body_bb = L.append_block context "while_body" lfunc in 
    let (_, while_body_builder) = build_stmt globals locals (L.builder_at_end context body_bb) lfunc sst in add_terminal while_body_builder (L.build_br pred_bb);
    let pred_builder = L.builder_at_end context pred_bb in 
    let bool_val = build_expr globals locals pred_builder se in 
    let merge_bb = L.append_block context "merge" lfunc in ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
    (locals, L.builder_at_end context merge_bb)
    | SFor(init, end_cond, trans_exp, sstmt_list) -> 
    let init_stmt = match init with 
      Some s -> s
    | None -> SExpr(Void, Noexpr) in
    let end_cond_expr = match end_cond with
      Some e -> e
    | None -> (A.Bool, SBoolLit(true)) (*No test because there is no break*)
    in let trans_stmt = match trans_exp with
      Some s -> s
    | None -> (A.Void, Noexpr) in 

      build_stmt globals locals builder lfunc (SBlock([init_stmt; SWhile(end_cond_expr, SBlock([sstmt_list; SExpr(trans_stmt)]))]))
  and build_stmt_list globals locals builder lfunc= (*lfunc is the function it belongs to, builder is the corresponding builder, sl is the last param *)
  function
    [] -> builder
    | s :: sl -> 
    let (new_locals, new_builder) = build_stmt globals locals builder lfunc s
    in build_stmt_list globals new_locals new_builder lfunc sl
  in

  
  (* Main block, the entry point of our program *)
  (* Define builder for the main program. It's what we refer to as main builder *)
  (* We prepare the globals and the locals hashtable for the main block. It starts with all empty. *)
  (* we use hashmap because we do not want to carry the StringMap around... So heavy! *)

  (* We also start by defining hashmap of globals, locals, ... *)

  (* Variable sections *)
  (* Variable map is a map from var name to var's addr (llvalue) *)
  (* We can use hashmap instead of StringMap for ease of mind *)
  (* Globals are vars outside of the current scope. Locals are inside the current scope *)
  let globals = StringMap.empty in
  let locals = StringMap.empty in

  let main = L.define_function "main" (L.function_type i32_t [||]) the_module in
  let builder = L.builder_at_end context (L.entry_block main) in
  let main_builder = build_stmt_list globals locals builder main program in
  let _ = add_terminal main_builder (L.build_ret (L.const_int i32_t 0)) in

  the_module

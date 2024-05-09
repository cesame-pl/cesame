module L = Llvm
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

let translate (program: struct_decl list * sstmt list) : Llvm.llmodule = 
  
  (* LLVM envs *)
  let context    = L.global_context () in
  let the_module = L.create_module context "cesame" in

  (* LLVM types *)
  let i32_t      = L.i32_type       context (* integer *)
  and f64_t      = L.double_type    context (* float   *)
  and char_t     = L.i8_type        context (* Char    *)
  and char_pt    = L.pointer_type   (L.i8_type context) (* Char* *)
  and bool_t     = L.i1_type        context (* Bool    *)
  in

  (* Return LLVM type for sast type *)
  let ltype_of_typ = function
      Int    -> i32_t
    | Float  -> f64_t
    | Bool   -> bool_t
    | Char   -> char_t
    | String -> char_pt
  in

  (* Function to determine the size of an array *)
  (* Takes sx as SArrayList l *)
  let size_of_arr = function
        SArrayLit l -> List.length l
      | _ -> raise (Failure "Excepted an array")
  in
  (* Recursive function to determine the LLVM type of an array *)
  (* Takes in sexpr *)
  let rec ltype_of_arr (t, sx) = 
    match t, sx with 
    | (Array el_type, SArrayLit l) (* element_type *) -> 
      let size = size_of_arr sx in 
      if size = 0 then 
        L.array_type (ltype_of_typ el_type) 0 
      else (* assume all the elements have the same size *)
        L.array_type (ltype_of_arr (List.hd l)) size
    | _ -> ltype_of_typ t
  in

  (* Function to add a terminal instruction *)
  let add_terminal builder instr =
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None   -> ignore (instr builder) 
  in

  (* Printf format string based on type *)
  let fmt_str_of_typ = function 
      Int    -> "%d\n"
    | Float  -> "%g\n"
    | String -> "%s\n"
    | _      -> raise (Failure "Unsupported type for print")
  in

  (* Declaration of printf function *)
  let printf_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type char_t |] in
  let printf_func : L.llvalue = 
    L.declare_function "printf" printf_t the_module in

  (* Recursive function to build LLVM IR for expressions *)
  (* Takes in an sexpr; Returns Llvalue *)
  let rec build_expr globals locals builder ((t, sx) : sexpr) : L.llvalue =
    match sx with
      Noexpr          -> L.const_int i32_t 0
    | SLiteral (i)    -> L.const_int i32_t i
    | SCharLit (c)    -> L.const_int (ltype_of_typ Char) (Char.code c)
    | SBoolLit (b)    -> L.const_int bool_t (if b then 1 else 0)
    | SFloatLit (f)   -> L.const_float f64_t f
    | SStrLit (s)     -> L.build_global_stringptr s "str" builder
    | SArrayLit (l)   -> 
      let arr_size = size_of_arr sx in 
      let arr_ptr = L.build_alloca (ltype_of_arr (t, sx)) "arr" builder in 
      for i = 0 to arr_size - 1 do 
        let el_ptr = L.build_in_bounds_gep arr_ptr [|L.const_int i32_t 0; L.const_int i32_t i|] "tmp" builder in
        let el_val = build_expr globals locals builder (List.nth l i) in
        ignore (L.build_store el_val el_ptr builder)
      done;
      arr_ptr;
    | SId (s)         -> L.build_load (var_addr_lookup globals locals builder sx) s builder
    | SUnaop (op, se) ->
      let e' = build_expr globals locals builder se in 
      (match op with 
        Not -> L.build_not e' "tmp" builder)
    | SBinop (se1, op, se2) ->
      (* check if se1 and se2 are valid in semant *)
      (* print_string (string_of_sexpr((t,  SBinop(se1, op, se2)))); *)
      let e1' = build_expr globals locals builder se1 in
      let e2' = build_expr globals locals builder se2 in
      let err = "Unsupported binary operation for this type" in
      (match fst se1 with
        Bool -> 
        (match op with 
          And   -> L.build_and
        | Or    -> L.build_or
        | Equal -> L.build_icmp L.Icmp.Eq
        | Neq   -> L.build_icmp L.Icmp.Ne
        | Ge    -> L.build_icmp L.Icmp.Sge
        | Le    -> L.build_icmp L.Icmp.Sle
        | Gt    -> L.build_icmp L.Icmp.Sgt
        | Lt    -> L.build_icmp L.Icmp.Slt)
      | Int ->
        (match op with 
          And   -> L.build_and
        | Or    -> L.build_or
        | Add   -> L.build_add
        | Sub   -> L.build_sub 
        | Mul   -> L.build_mul
        | Div   -> L.build_sdiv (* signed division *)
        | Mod   -> L.build_srem (* signed remainder *)
        | Equal -> L.build_icmp L.Icmp.Eq
        | Neq   -> L.build_icmp L.Icmp.Ne
        | Ge    -> L.build_icmp L.Icmp.Sge
        | Le    -> L.build_icmp L.Icmp.Sle
        | Gt    -> L.build_icmp L.Icmp.Sgt
        | Lt    -> L.build_icmp L.Icmp.Slt)
      | Float -> 
        (match op with
          And   -> L.build_and
        | Or    -> L.build_or
        | Add   -> L.build_fadd
        | Sub   -> L.build_fsub 
        | Mul   -> L.build_fmul
        | Div   -> L.build_fdiv
        | Equal -> L.build_fcmp L.Fcmp.Oeq
        | Neq   -> L.build_fcmp L.Fcmp.One
        | Ge    -> L.build_fcmp L.Fcmp.Oge
        | Le    -> L.build_fcmp L.Fcmp.Ole
        | Gt    -> L.build_fcmp L.Fcmp.Ogt
        | Lt    -> L.build_fcmp L.Fcmp.Olt) 
      | _ -> raise (Failure err)) e1' e2' "tmp" builder
    | SAssign (se1, se2) -> 
      let e1' = var_addr_lookup globals locals builder (snd se1) in 
      let e2' = build_expr globals locals builder se2 in
      ignore(L.build_store e2' e1' builder); e2'
    | SCall ("print", [se]) ->
      let printf_format = L.build_global_stringptr (fmt_str_of_typ (fst se)) "fmt" builder in
      L.build_call printf_func [| printf_format; (build_expr globals locals builder se) |] "printf" builder
    | SAccessEle (se1, se2) -> 
      (* TODO: test more complicated se like student.courses[0] *)
      let e1' = build_expr globals locals builder se1 in 
      let e2' = build_expr globals locals builder se2 in 
      let el_ptr = L.build_in_bounds_gep e1' [|L.const_int i32_t 0; e2'|] "tmp" builder in 
      L.build_load el_ptr "tmp" builder

  (* Helper function to look up variable from both the global and local scope (local first) *)
  (* Takes in sx; Returns llvalue *)
  and var_addr_lookup globals locals builder sx : L.llvalue = 
    match sx with 
      SId (s) -> 
      (try StringMap.find s locals
        with Not_found -> StringMap.find s globals)
    | SAccessEle (se1, se2) ->
      let e1' = build_expr globals locals builder se1 in 
      let e2' = build_expr globals locals builder se2 in 
      L.build_in_bounds_gep e1' [|L.const_int i32_t 0; e2'|] "tmp" builder
    (* TODO: AccessMember *)
  in 

  (* Recursive function to build LLVM IR for statements *)
  let rec build_stmt globals locals builder lfunc = function
      SExpr (se) -> 
      ignore(build_expr globals locals builder se); 
      (locals, builder)
    (* If it's a declaration, insert it into the locals. The bind is the first element of locals *)
    (* Why do we need local_func_decls here? *)
    (* TODO: If t is a Func type? *)
    | SVDecl (t, var_name, None) -> 
      let lhs_addr = match t with
        (* TODO: ADD functions *)
        | _ -> L.build_alloca (ltype_of_typ t) var_name builder
      in
      (* Add var_name -> var_addr to the map *)
      let new_locals = StringMap.add var_name lhs_addr locals in 
      (new_locals, builder)
    | SVDecl (t, var_name, Some (rt, re)) -> (* right type and right expression *)
      let ltype = match t with 
        (* TODO: ADD functions *)
          Array _ -> L.pointer_type (ltype_of_arr (rt, re))
        | _ -> ltype_of_typ rt
      in
      let lhs_addr = L.build_alloca ltype var_name builder in
      let new_locals = StringMap.add var_name lhs_addr locals in (* Add var_name -> var_addr to the map *)
      let e' = build_expr globals locals builder (rt, re) in
      ignore(L.build_store e' lhs_addr builder);
      (new_locals, builder)
    | SBlock (sstmt_l) -> 
      let new_builder = build_stmt_list (combine_maps globals locals) StringMap.empty builder lfunc sstmt_l in 
      (locals, new_builder)
    | SIf (se_sst_l, sst) -> 
      let rec build_if globals locals builder = function
          ([] , sst) -> 
          let (_, new_builder) = build_stmt globals locals builder lfunc sst in 
          new_builder 
        | ((fse, then_sst) :: else_sl, sst) -> 
          let bool_val = build_expr globals locals builder fse in 
          let merge_bb = L.append_block context "merge" lfunc in 
          let b_br_merge = L.build_br merge_bb in 
          let then_bb = L.append_block context "then" lfunc in 
          let (_, then_stmt_builder) = build_stmt globals locals (L.builder_at_end context then_bb) lfunc then_sst in 
          add_terminal then_stmt_builder b_br_merge; 
          let else_bb = L.append_block context "else" lfunc in 
          let else_stmt_builer = build_if globals locals (L.builder_at_end context else_bb) (else_sl, sst) in 
          add_terminal else_stmt_builer b_br_merge;
          ignore(L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context merge_bb
      in
      (locals, build_if globals locals builder (List.rev se_sst_l, sst))
    | SWhile (se, sst) -> 
      let pred_bb = L.append_block context "while" lfunc in 
      ignore(L.build_br pred_bb builder);
      let body_bb = L.append_block context "while_body" lfunc in 
      let (_, while_body_builder) = build_stmt globals locals (L.builder_at_end context body_bb) lfunc sst in 
      add_terminal while_body_builder (L.build_br pred_bb);
      let pred_builder = L.builder_at_end context pred_bb in 
      let bool_val = build_expr globals locals pred_builder se in 
      let merge_bb = L.append_block context "merge" lfunc in ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
      (locals, L.builder_at_end context merge_bb)
    | SFor (init, end_cond, trans_exp, sstmt_list) -> 
      let init_stmt = match init with 
          Some s -> s
        | None -> SExpr(Void, Noexpr) 
      in
      let end_cond_expr = match end_cond with
          Some e -> e
        | None -> (Bool, SBoolLit(true)) (*No test because there is no break*)
      in 
      let trans_stmt = match trans_exp with
          Some s -> s
        | None -> (Void, Noexpr) in 
      build_stmt globals locals builder lfunc (SBlock([init_stmt; SWhile(end_cond_expr, SBlock([sstmt_list; SExpr(trans_stmt)]))]))
  
  and build_stmt_list globals locals builder lfunc = function (*lfunc is the function it belongs to, builder is the corresponding builder, sl is the last param *)
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
  let (program_sdecl_list, program_stmt_list) = program in
  let main_builder = build_stmt_list globals locals builder main program_stmt_list in
  let _ = add_terminal main_builder (L.build_ret (L.const_int i32_t 0)) in

  the_module

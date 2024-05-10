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

  (* struct_tbl : { struct_name : (struct_type, struct_decl)}*)
  let struct_tbl = Hashtbl.create 1000 in
  let struct_lookup name = 
    try Hashtbl.find struct_tbl name 
      with Not_found -> raise (Failure ("Struct type '" ^ name ^ "' not found"))
  in
  let struct_member_index member_name struct_decl =
    let rec find_index index = function
      | [] -> raise (Failure ("Struct member '" ^ member_name ^ "' not found")) (* Member not found *)
      | (_, name) :: rest ->
          if name = member_name then index (* Return the index if the member is found *)
          else find_index (index + 1) rest
    in
    find_index 0 struct_decl.body
  in

  (* Return LLVM type for sast type *)
  let rec ltype_of_typ = function
      Int     -> i32_t
    | Float   -> f64_t
    | Bool    -> bool_t
    | Char    -> char_t
    | String  -> char_pt
    | Array t -> L.pointer_type (ltype_of_typ t)
    | Struct s ->
      let (_, sd) = struct_lookup s in
      let el_types = Array.of_list (List.map (fun (t, _) -> ltype_of_typ t) sd.body) in
      L.struct_type context el_types
  in
  (* Return LLVM const value for ast type; Used in struct var init *)
  let lconst_of_typ = function 
    | Int -> L.const_int (ltype_of_typ Int) 0
    | Float -> L.const_float (ltype_of_typ Float) 0.0
    | Bool -> L.const_int (ltype_of_typ Bool) 0
    | Char -> L.const_int (ltype_of_typ Char) 0
    | String ->  L.const_stringz context ""
    | Array t -> L.const_pointer_null (ltype_of_typ t)
    | Struct _ -> L.const_pointer_null (L.struct_type context [||])
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

  let func_of_builder builder = L.block_parent (L.insertion_block builder) in

  (* Declaration of printf function *)
  let printf_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type char_t |] in
  let printf_func : L.llvalue = 
    L.declare_function "printf" printf_t the_module in

  (* Recursive function to build LLVM IR for expressions *)
  (* Takes in an sexpr; Returns Llvalue *)
  let rec build_expr vars func_decls builder ((t, sx) : sexpr) : L.llvalue =
    match sx with
      Noexpr          -> L.const_int i32_t 0
    | SLiteral (i)    -> L.const_int i32_t i
    | SCharLit (c)    -> L.const_int (ltype_of_typ Char) (Char.code c)
    | SBoolLit (b)    -> L.const_int bool_t (if b then 1 else 0)
    | SFloatLit (f)   -> L.const_float f64_t f
    | SStrLit (s)     -> L.build_global_stringptr s "str" builder
    | SArrayLit (l)   -> 
      let arr_size = List.length l and arr_type = ltype_of_typ t in
      let arr_alloca = L.build_array_malloc arr_type (L.const_int i32_t arr_size) "arr" builder in
      let arr_ptr = L.build_pointercast arr_alloca arr_type "arr_ptr" builder in
      for i = 0 to arr_size - 1 do 
        let el_ptr = L.build_in_bounds_gep arr_ptr [|L.const_int i32_t i|] "tmp" builder in
        let el_val = build_expr vars func_decls builder (List.nth l i) in
        ignore (L.build_store el_val el_ptr builder)
      done;
      arr_ptr
    | SId (s)         -> L.build_load (var_addr_lookup vars func_decls builder sx) s builder
    | SUnaop (op, se) ->
      let e' = build_expr vars func_decls builder se in 
      (match op with 
        Not -> L.build_not e' "tmp" builder)
    | SBinop (se1, op, se2) ->
      (* check if se1 and se2 are valid in semant *)
      (* print_string (string_of_sexpr((t,  SBinop(se1, op, se2)))); *)
      let e1' = build_expr vars func_decls builder se1 in
      let e2' = build_expr vars func_decls builder se2 in
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
      let e1' = var_addr_lookup vars func_decls builder (snd se1) in 
      let e2' = build_expr vars func_decls builder se2 in
      ignore(L.build_store e2' e1' builder); e2'
    | SCall ("print", [se]) ->
      let printf_format = L.build_global_stringptr (fmt_str_of_typ (fst se)) "fmt" builder in
      L.build_call printf_func [| printf_format; (build_expr vars func_decls builder se) |] "printf" builder
    | SCall (f, sel) -> (* fname: string, args: se list *)
      let func_ptr = func_addr_lookup func_decls f in 
      let func_args = List.rev (List.map (build_expr vars func_decls builder) (List.rev sel)) in
      L.build_call func_ptr (Array.of_list func_args) (f ^ "_result") builder
    | SAccessMember _ | SAccessEle _ -> 
      let sub_ptr = var_addr_lookup vars func_decls builder sx in 
      L.build_load sub_ptr "tmp" builder

  (* Helper function to look up variable from both the global and local scope (local first) *)
  (* Takes sx; Returns llvalue *)
  and var_addr_lookup vars func_decls builder sx : L.llvalue = 
    (match sx with 
      SId (s) -> StringMap.find s vars
    | SAccessMember ((Struct sname, sx1), (_, SId s2)) -> 
      let (_, struct_decl) = struct_lookup sname in
      let struct_ptr = (match sx1 with 
        | SId s -> 
          var_addr_lookup vars func_decls builder sx1
        | _ -> 
          build_expr vars func_decls builder (Struct sname, sx1))
      in
      let member_index = struct_member_index s2 struct_decl in 
      L.build_struct_gep struct_ptr member_index "tmp" builder 
    | SAccessEle (se1, se2) -> 
      let e1' = (match se1 with 
        | _, SId s -> 
          L.build_load (var_addr_lookup vars func_decls builder (snd se1)) "tmp" builder
        | _, _ -> 
          build_expr vars func_decls builder se1) 
      in
      let e2' = build_expr vars func_decls builder se2 in
      L.build_in_bounds_gep e1' [|e2'|] "tmp" builder)
  (* Helper function to look up functions from both the global and local scope (local first) *)
  (* Takes string; Returns llvalue *)
  and func_addr_lookup func_decls fname : L.llvalue = StringMap.find fname func_decls 
  in

  (* Recursive function to build LLVM IR for statements *)
  let rec build_stmt globals locals global_func_decls local_func_decls builder sexpr = 
    (* globals and locals won't change after being combined *)
    (* vars: all variables; func_decls: all functions declarations *)
    let vars = combine_maps globals locals and func_decls = combine_maps global_func_decls local_func_decls in

    match sexpr with 
      SExpr (se) -> 
      ignore(build_expr vars func_decls builder se); 
      (locals, local_func_decls, builder)
    
    (* If it's a declaration, insert it into the locals. The bind is the first element of locals *)
    (* TODO: If t is a Func type? *)
    | SVDecl (t, var_name, se_opt) ->
      let lhs_addr = match se_opt with
          None -> 
            L.build_alloca (ltype_of_typ t) var_name builder
        | Some se -> (* right type and right expression *)
            let addr = L.build_alloca (ltype_of_typ t) var_name builder in
            let e' = build_expr vars func_decls builder se in
            ignore(L.build_store e' addr builder);
            addr
      in
      let new_locals = StringMap.add var_name lhs_addr locals in 
      (new_locals, local_func_decls, builder)

    | SBlock (sstmt_l) -> 
      let new_builder = build_stmt_list vars StringMap.empty func_decls StringMap.empty builder sstmt_l in 
      (locals, local_func_decls, new_builder)

    | SIf (se_sst_l, sst) -> 
      let rec build_if globals locals builder = function
          ([] , sst) -> 
          let (_, _, new_builder) = build_stmt globals locals global_func_decls local_func_decls builder sst in 
          new_builder 
        | ((fse, then_sst) :: else_sl, sst) -> 
          let bool_val = build_expr vars func_decls builder fse in 
          let merge_bb = L.append_block context "merge" (func_of_builder builder) in 
          let b_br_merge = L.build_br merge_bb in 
          let then_bb = L.append_block context "then" (func_of_builder builder) in 
          let (_, _, then_stmt_builder) = build_stmt globals locals global_func_decls local_func_decls (L.builder_at_end context then_bb) then_sst in 
          add_terminal then_stmt_builder b_br_merge; 
          let else_bb = L.append_block context "else" (func_of_builder builder) in 
          let else_stmt_builer = build_if globals locals (L.builder_at_end context else_bb) (else_sl, sst) in 
          add_terminal else_stmt_builer b_br_merge;
          ignore(L.build_cond_br bool_val then_bb else_bb builder);
          L.builder_at_end context merge_bb
      in
      (locals, local_func_decls, build_if globals locals builder (List.rev se_sst_l, sst))

    | SWhile (se, sst) -> 
      let pred_bb = L.append_block context "while" (func_of_builder builder) in 
      ignore(L.build_br pred_bb builder);
      let body_bb = L.append_block context "while_body" (func_of_builder builder) in 
      let (_, _, while_body_builder) = build_stmt globals locals global_func_decls local_func_decls (L.builder_at_end context body_bb) sst in 
      add_terminal while_body_builder (L.build_br pred_bb);
      let pred_builder = L.builder_at_end context pred_bb in 
      let bool_val = build_expr vars func_decls pred_builder se in 
      let merge_bb = L.append_block context "merge" (func_of_builder builder) in ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
      (locals, local_func_decls, L.builder_at_end context merge_bb)

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
      let new_block = SBlock([init_stmt; SWhile(end_cond_expr, SBlock([sstmt_list; SExpr(trans_stmt)]))]) in
      build_stmt globals locals global_func_decls local_func_decls builder new_block
    
    | SFDef (fd) ->
      (* TODO: array or struct as params *)
      (* Define the function (arguments and return type) *)
      let param_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fd.sparams) in 
      let ftype = L.function_type (ltype_of_typ fd.srtyp) param_types in 
      let func_ptr = L.define_function fd.sfname ftype the_module in
      let func_builder = L.builder_at_end context (L.entry_block func_ptr) in

      (* Construct the function's globals & locals & func decls *)
      let func_locals = 
        let add_param m (t, n) p =
          L.set_value_name n p;
          let local = L.build_alloca (ltype_of_typ t) n func_builder in
          ignore (L.build_store p local func_builder);
          StringMap.add n local m
        in
        List.fold_left2 add_param StringMap.empty fd.sparams (Array.to_list (L.params func_ptr))
      in
      let func_local_func_decls = StringMap.add fd.sfname func_ptr StringMap.empty in

      (* Build the function body *)
      let new_builder = build_stmt_list vars func_locals func_decls func_local_func_decls func_builder fd.sbody in
      let ret = (match fd.srtyp with 
          Void -> L.build_ret_void 
        | _ -> L.build_ret (L.const_int (ltype_of_typ fd.srtyp) 0) )
      in
      ignore(add_terminal new_builder ret);

      (* Returns the original locals and builder, but new local func decls *)
      let new_local_func_decls = StringMap.add fd.sfname func_ptr local_func_decls in
      (locals, new_local_func_decls, builder)

    | SReturn se -> 
      ignore(L.build_ret (build_expr vars func_decls builder se) builder);
      (locals, local_func_decls, builder)
  
  and build_stmt_list globals locals global_func_decls local_func_decls builder = function
      [] -> builder
    | s :: sl -> 
      let (new_locals, new_local_func_decls, new_builder) = build_stmt globals locals global_func_decls local_func_decls builder s in
      build_stmt_list globals new_locals global_func_decls new_local_func_decls new_builder sl
  in

  (* Function to build global struct declarations list *)
  let build_struct_decl_list struct_decls =
    List.iter (fun sd ->
      (* Define the struct type with a pointer to the array element *)
      let el_types = List.map (fun (t, _) -> ltype_of_typ t) sd.body in
      let struct_type = L.struct_type context (Array.of_list el_types) in
      Hashtbl.add struct_tbl sd.sname (struct_type, sd);

      (* Get the member init values *)
      let null_initializer =
        match sd.body with
        | [] -> L.const_null struct_type
        | _ :: _ -> 
          let member_nulls = List.map (fun (t, _) -> lconst_of_typ t) sd.body in
          L.const_named_struct struct_type (Array.of_list member_nulls)
      in

      (* Define a global variable of this struct type *)
      let _ = L.define_global sd.sname null_initializer the_module in
      ()
    ) struct_decls
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
  let globals = StringMap.empty and locals = StringMap.empty in
  let global_func_decls = StringMap.empty and local_func_decls = StringMap.empty in

  let main = L.define_function "main" (L.function_type i32_t [||]) the_module in
  let _ = build_struct_decl_list (fst program) in
  let builder = L.builder_at_end context (L.entry_block main) in
  let main_builder = build_stmt_list globals locals global_func_decls local_func_decls builder (snd program) in
  let _ = add_terminal main_builder (L.build_ret (L.const_int i32_t 0)) in

  the_module

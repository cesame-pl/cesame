module L = Llvm
module A = Ast
open Sast 

open Hashtbl

(* C Array v.s. Java Array; C char* v.s. Java String *)

type string2addr_t = (string, L.llvalue) Hashtbl.t

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
    try Hashtbl.find locals var_name
      with Not_found -> Hashtbl.find globals var_name
  in

  let rec build_expr globals locals builder ((_, e) : sexpr) = match e with
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
    (* TODO: Binop *)
    (* | SBinop (e1, op, e2) ->
      let e1_val = build_expr builder e1 in
      let e2_val = build_expr builder e2 in
      () *)
  in

  let rec build_stmt globals locals builder = function
    SExpr(se) -> ignore(build_expr globals locals builder se); builder
    (* If it's a declaration, insert it into the locals. The bind is the first element of locals *)
    (* Why do we need local_func_decls here? *)
    (* TODO: If t is a Func type? *)
    | SVDecl (t, var_name, None) -> 
      let lhs_addr = match t with
        (* TODO: ADD functions *)
        | _ -> L.build_alloca (ltype_of_typ t) var_name builder
      in ignore (Hashtbl.add locals var_name lhs_addr); (* Add var_name -> var_addr to the map *)
      builder
    | SVDecl (t, var_name, Some (rt, re)) -> (* right type and right expression *)
      let lhs_addr = match t with
        (* TODO: ADD functions *)
        | _ -> L.build_alloca (ltype_of_typ t) var_name builder
      in ignore(Hashtbl.add locals var_name lhs_addr); (* The semicolon is crucial here! *) (* Add var_name -> var_addr to the map *)
      let e' = build_expr globals locals builder (rt, re) (* Because of the semicolon, we don't need "in" here *)
      in ignore(L.build_store e' lhs_addr builder);
      builder
  in

  let rec build_stmt_list globals locals builder = (* sl is the last param *)
  function
    [] -> builder
    | s :: sl -> 
    ignore(build_stmt globals locals builder s);
    build_stmt_list globals locals builder sl
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
  let (globals : string2addr_t) = Hashtbl.create 500 in
  let (locals : string2addr_t) = Hashtbl.create 500 in

  let main = L.define_function "main" (L.function_type i32_t [||]) the_module in
  let builder = L.builder_at_end context (L.entry_block main) in
  let main_builder = build_stmt_list globals locals builder program in
  let _ = add_terminal main_builder (L.build_ret (L.const_int i32_t 0)) in

  the_module

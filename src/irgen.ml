module L = Llvm
module A = Ast
open Sast 

let translate (program: sstmt list) : Llvm.llmodule = 
  
  (* llvm envs *)
  let context = L.global_context () in
  let the_module = L.create_module context "cesame" in

  (* llvm types *)
  let i32_t      = L.i32_type       context (* integer *)
  and f64_t      = L.double_type    context (* float   *)
  and i8_t       = L.i8_type        context (* Char    *)
  in

  (* return llvm type for sast type *)
  let ltype_of_typ = function
    | A.Int -> i32_t
    | A.Float -> f64_t
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
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
    L.declare_function "printf" printf_t the_module in

  let printbig_t : L.lltype =
    L.function_type i32_t [| i32_t |] in
  let printbig_func : L.llvalue =
    L.declare_function "printbig" printbig_t the_module in

  let rec build_expr builder ((_, e) : sexpr) = function
    | SLiteral(i)         -> L.const_int i32_t i
    | SFloatLit(f)        -> L.const_float f64_t f
    | SCall ("print", [e])
    | SCall ("printb", [e]) ->
	    L.build_call printf_func [| int_format_str builder; (build_expr builder e) |]
	    "printf" builder
    | SCall ("printbig", [e]) ->
	    L.build_call printbig_func [| (build_expr builder e) |] "printbig" builder
    | SCall ("printf", [e]) -> 
	    L.build_call printf_func [| float_format_str ; (build_expr builder e) |]
	    "printf" builder
  in

  let build_stmt builder = function
    | SExpr(se) -> build_expr builder se
  in


  let build_stmt_list builder = (* sl is the last param *)
  function
    | [] -> builder
    | s :: sl -> 
    let b = build_stmt builder in
    build_stmt_list b sl
  in

  
  (* Main block, the entry point of our program *)
  (* Define builder for the main program. It's what we refer to as main builder *)
  (* We prepare the globals and the locals hashtable for the main block. It starts with all empty. *)

  let main = L.define_function "main" (L.function_type i32_t [||]) the_module in
  let builder = L.builder_at_end context (L.entry_block main) in
  let main_builder = build_stmt_list builder program in
  let _ = add_terminal main_builder (L.build_ret (L.const_int i32_t 0)) in

  the_module

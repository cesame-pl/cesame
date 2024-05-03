module L = Llvm
module A = Ast
open Sast 

let translate (stmts) = 
  let context = L.global_context() in
  let the_module = L.create_module context "Cesame" in

  the_module
open Compiler
open Testtool

let decorate an = "(" ^ an ^ ")"

let () =
  let code = 
    TestUtil.source2 
    |> Ast.make_ast
    |> Ast.normalize
    |> Cps.cps_trans
    |> AdmBetaReduction.normalize
    |> ANormalization.a_normalize
    |> ANormalization.normalize_binds
    |> ANormalization.ast_of_anf
    |> Ast.code_of_ast
    |> decorate 
  in
  TestUtil.print_label "A-Normalized";
  print_endline code

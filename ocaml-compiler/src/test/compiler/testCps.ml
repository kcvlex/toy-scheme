open Compiler
open Testtool

let decorate cps_code = "(" ^ cps_code ^ " (lambda (x) x))"

let () =
  let code = 
    TestUtil.source2 
    |> Ast.make_ast
    |> Ast.normalize
    |> Cps.cps_trans
    |> Cps.ast_of_cps
    |> Ast.code_of_ast
    |> decorate
  in
  TestUtil.print_label "CPS";
  print_endline code

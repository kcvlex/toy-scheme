open Compiler
open Testtool

let () =
  let code =
    TestUtil.source2 
    |> Ast.make_ast
    |> Ast.normalize
    |> Ast.code_of_ast
  in
  TestUtil.print_label "AST";
  print_endline code

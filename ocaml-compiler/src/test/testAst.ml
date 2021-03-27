open Compiler
open TestUtil

let () =
  let ast = make_ast source2 in
  let code =
    ast
    |> Ast.normalize ast
    |> Ast.code_of_ast ast
  in
  print_label "AST"; print_endline code

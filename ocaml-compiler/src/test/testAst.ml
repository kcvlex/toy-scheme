open Compiler
open TestUtil

let () =
  let code =
    source2 |> Ast.make_ast
            |> Ast.normalize
            |> Ast.code_of_ast
  in
  print_label "AST"; print_endline code

open Compiler
open TestUtil

let decorate an = "(display (" ^ an ^ "))"
 
let () =
  let code = 
    source4 |> Ast.make_ast
            |> Ast.normalize
            |> Cps.cps_trans
            |> AdmBetaReduction.normalize
            |> ANormalization.a_normalize
            |> ANormalization.merge_lets
            |> ANormalization.ast_of_anf
            |> Ast.code_of_ast
            |> decorate
  in
  print_label "A-Normalized"; print_endline code

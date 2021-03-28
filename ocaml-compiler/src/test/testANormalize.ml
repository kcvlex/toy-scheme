open Compiler
open TestUtil

let decorate cps_code = "(" ^ cps_code ^ " display)"
 
let () =
  let code = 
    source2 |> Ast.make_ast
            |> Ast.normalize
            |> Cps.cps_trans
            |> AdmBetaReduction.normalize
            |> ANormalization.a_normalize
            |> ANormalization.ast_of_anorm
            |> Ast.code_of_ast
            |> decorate
  in
  print_label "A-Normalized"; print_endline code

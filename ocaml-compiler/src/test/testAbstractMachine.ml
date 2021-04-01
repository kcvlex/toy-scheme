open Compiler
open TestUtil

let () =
  let code = 
    source3 |> Ast.make_ast
            |> Ast.normalize
            |> Cps.cps_trans
            |> AdmBetaReduction.normalize
            |> ANormalization.a_normalize
            |> ANormalization.normalize_binds
            |> Closure.closure_trans
            |> AbstractMachine.translate
            |> AbstractMachine.eval
  in
  print_label "Abstract Machine"

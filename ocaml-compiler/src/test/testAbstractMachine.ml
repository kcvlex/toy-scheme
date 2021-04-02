open Compiler
open TestUtil

let print_program machine = 
  machine |> AbstractMachine.string_of_program 
          |> print_endline

let () =
  print_label "Abstract Machine";
  let machine =
  source3 |> Ast.make_ast
          |> Ast.normalize
          |> Cps.cps_trans
          |> AdmBetaReduction.normalize
          |> ANormalization.a_normalize
          |> ANormalization.normalize_binds
          |> Closure.closure_trans
          |> AbstractMachine.translate
  in
  print_program machine;
  AbstractMachine.eval machine

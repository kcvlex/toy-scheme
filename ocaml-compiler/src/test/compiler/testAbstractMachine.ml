open Compiler
open Testtool

let print_program machine = 
  let oc = open_out "/home/taroy/toy-scheme/ocaml-compiler/src/abs2.txt" in
  machine |> AbstractMachine.string_of_machine 
          |> Printf.fprintf oc "%s\n";
  close_out oc

let () =
  TestUtil.print_label "Abstract Machine";
  let machine =
    TestUtil.source3 
    |> Ast.make_ast
    |> Ast.normalize
    |> Cps.cps_trans
    |> AdmBetaReduction.normalize
    |> ANormalization.a_normalize
    |> ANormalization.normalize_binds
    |> Closure.closure_trans
    |> AbstractMachine.translate
    |> AbstractMachine.make_machine
  in
  print_program machine;
  AbstractMachine.eval machine

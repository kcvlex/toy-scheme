open Compiler
open Assm
open Testtool

(*
let () =
  ThreeAddressCode.set_logging false;
  RegAlloc.set_logging false;
  let reg_num = (7, 10, 9) in
  let sim =
    TestUtil.source2
    |> Ast.make_ast
    |> Ast.normalize
    |> Cps.cps_trans
    |> AdmBetaReduction.normalize
    |> ANormalization.a_normalize
    |> ANormalization.normalize_binds
    |> Closure.closure_trans
    |> AbstractMachine.translate
    |> ThreeAddressCode.from_abs_program
    |> RegAlloc.allocate reg_num
    |> RiscvAssmGen.generate
    |> List.append (List.flatten Builtin.builtin_list)
    |> Reduction.reduction
    |> Reduction.convert
    |> Simulator.init reg_num
  in
  TestUtil.print_label "Simulator";
  print_endline (Simulator.dump_regfile sim);
  Simulator.eval sim;
  print_endline (Simulator.dump_regfile sim)
  *)

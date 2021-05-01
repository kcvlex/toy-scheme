open Compiler
open Assm
open Testtool

let () =
  ThreeAddressCode.set_logging false;
  RegAlloc.set_logging false;
  let reg_num = (7, 11, 8) in
  let assm =
    TestUtil.source3
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
    |> List.append Builtin.lib
    |> RiscvAssmGen.string_of_assm
  in
  TestUtil.print_label "Assembly";
  let oc = open_out "/home/taroy/toy-scheme/ocaml-compiler/src/test/assm/asm.s" in
  Printf.fprintf oc "%s\n" assm;
  close_out oc

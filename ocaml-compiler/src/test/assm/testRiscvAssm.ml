open Compiler
open Assm
open Testtool

let gen32 thc =
  let assm =
    thc
    |> RiscvAssmGen.generate Riscv.RV32I Riscv.FPGA
    |> RiscvAssmGen.link_builtin
    |> RiscvAssmGen.string_of_assm
  in
  TestUtil.print_label "RV32I";
  let oc = open_out "/home/taroy/toy-scheme/ocaml-compiler/src/test/assm/asm32.s" in
  Printf.fprintf oc "%s\n" assm;
  close_out oc

let gen64 thc =
  let assm =
    thc
    |> RiscvAssmGen.generate Riscv.RV64I Riscv.Simulate
    |> RiscvAssmGen.link_builtin
    |> RiscvAssmGen.string_of_assm
  in
  TestUtil.print_label "RV64I";
  let oc = open_out "/home/taroy/toy-scheme/ocaml-compiler/src/test/assm/asm64.s" in
  Printf.fprintf oc "%s\n" assm;
  close_out oc

let () =
  ThreeAddressCode.set_logging false;
  RegAlloc.set_logging false;
  let reg_num = (7, 11, 8) in
  let thc =
    TestUtil.source6
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
  in
  gen32 thc; gen64 thc

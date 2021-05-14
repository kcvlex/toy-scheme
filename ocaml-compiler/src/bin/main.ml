open Compiler
open Assm

let input_file = ref ""
let output_file = ref ""
let arch = ref Riscv.RV32I
let mode = ref Riscv.FPGA

let set_arch s =
  let s = String.uppercase_ascii s in
  match s with
    | "RV32I" -> arch := Riscv.RV32I
    | "RV64I" -> arch := Riscv.RV64I
    | _ -> raise (Invalid_argument s)

let set_mode s =
  let s = String.uppercase_ascii s in
  match s with
    | "FPGA" -> mode := Riscv.FPGA
    | "SIMULATE" -> mode := Riscv.Simulate
    | _ -> raise (Invalid_argument s)

let speclist = [
  ("-o", Arg.String (fun s -> output_file := s), ": output filename");
  ("-arch", Arg.String (fun s -> set_arch s), ": rv32i or rv64i");
  ("-mode", Arg.String (fun s -> set_mode s), ": simulate or FPGA")
]

let parse () = Arg.parse speclist (fun s -> input_file := s) "usage : [input] -o [output]"

let () =
  ThreeAddressCode.set_logging false;
  RegAlloc.set_logging false;
  parse();
  let src =
    let ic = open_in !input_file in
    let len = in_channel_length ic in
    let s = really_input_string ic len in
    close_in ic;
    s
  in
  let reg_num = (7, 11, 8) in
  let assm =
    src
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
    |> RiscvAssmGen.generate !arch !mode
    |> RiscvAssmGen.link_builtin
    |> RiscvAssmGen.string_of_assm
  in
  let oc = open_out !output_file in
  Printf.fprintf oc "%s\n" assm;
  close_out oc

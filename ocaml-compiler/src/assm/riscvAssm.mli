type assm_instr_type =
  | Pseudo of PseudoInstr.t
  | Actual of ActualInstr.t

type line_type =
  | Instr of assm_instr_type
  | Ops of Riscv.ops_type
  | Label of string

type t = line_type list

type context_type = {
  mutable arch : Riscv.arch_type;
  mutable build : Riscv.build_type;
  mutable wsize : int;
  mutable wsize_log : int;
  mutable sentinel_num : int;
}

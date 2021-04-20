type assm_instr_type =
  | Pseudo of PseudoInstr.t
  | Actual of ActualInstr.t

type line_type =
  | Instr of assm_instr_type
  | Ops of Rv32i.ops_type
  | Label of string

type t = line_type list

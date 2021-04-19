(*
type t = 
  | LUI    of { dst : reg_type; imm: int; }
  | AUIPC  of { dst : reg_type; imm : int; }
  | JAL    of { dst : reg_type; offset : int; }
  | JALR   of { base : reg_type; dst : reg_type; offset : int; }
  | BEQ    of branch_type
  | BNE    of branch_type
  | BLT    of branch_type
  | BGE    of branch_type
  | BLTU   of branch_type
  | BGEU   of branch_type
  | LB     of load_type
  | LH     of load_type
  | LW     of load_type
  | LBU    of load_type
  | LHU    of load_type
  | SB     of store_type
  | SH     of store_type
  | SW     of store_type
  | ADDI   of op_imm_type
  | SLTI   of op_imm_type
  | SLTIU  of op_imm_type
  | XORI   of op_imm_type
  | ORI    of op_imm_type
  | ANDI   of op_imm_type
  | SLLI   of op_imm_type
  | SRLI   of op_imm_type
  | SRAI   of op_imm_type
  | ADD    of op_type
  | SUB    of op_type
  | SLL    of op_type
  | SLT    of op_type
  | SLTU   of op_type
  | XOR    of op_type
  | SRL    of op_type
  | SRA    of op_type
  | OR     of op_type
  | AND    of op_type
and branch_type = { lhs : reg_type; rhs : reg_type; offset : int; }
and load_type = { base : reg_type; dst : reg_type; offset : int; }
and store_type = { base : reg_type; src : reg_type; offset : int; }
and op_imm_type = { lhs : reg_type; rhs : int; dst : reg_type; }
and op_type = { lhs : reg_type; rhs : reg_type; dst : reg_type; }
open RiscvInstr

type func_seq = {
  label : string;
  seq : RiscvInstr.t list;
}

type frame = {
  mutable stack : string list;
  mutable register : reg_type list;
  mutable name2reg : (string, reg_type) Hashtbl.t;
}



type t =
  | LUI    of u_type
  | AUIPC  of u_type
  | JAL    of j_type
  | JALR   of i_type
  | BEQ    of b_type
  | BNE    of b_type
  | BLT    of b_type
  | BGE    of b_type
  | BLTU   of b_type
  | BGEU   of b_type
  | LB     of i_type
  | LH     of i_type
  | LW     of i_type
  | LBU    of i_type
  | LHU    of i_type
  | SB     of s_type
  | SH     of s_type
  | SW     of s_type
  | ADDI   of i_type
  | SLTI   of i_type
  | SLTIU  of i_type
  | XORI   of i_type
  | ORI    of i_type
  | ANDI   of i_type
  | SLLI   of r_type
  | SRLI   of r_type
  | SRAI   of r_type
  | ADD    of r_type
  | SUB    of r_type
  | SLL    of r_type
  | SLT    of r_type
  | SLTU   of r_type
  | XOR    of r_type
  | SRL    of r_type
  | SRA    of r_type
  | OR     of r_type
  | AND    of r_type
*)

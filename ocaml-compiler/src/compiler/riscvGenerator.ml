(*
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

type reg_type =
  | ZERO
  | RA
  | SP
  | FP
  | Tmp of int
  | Arg of int
  | CalleeSaved of int

type r_type = {
  rs1 : reg_type;
  rs2 : reg_type;
  rd : reg_type;
  funct3 : int;
  funct7 : int;
  opcode : int;
}
type i_type = {
  rs1 : reg_type;
  rd : reg_type;
  imm : int;
  funct3 : int;
  opcode : int;
}
type s_type = {
  rs1 : reg_type;
  rs2 : reg_type;
  imm : int;
  funct3 : int;
  opcode : int;
}
type b_type = {
  rs1 : reg_type;
  rs2 : reg_type;
  imm : int;
  funct3 : int;
  opcode : int;
}
type u_type = {
  rd : reg_type;
  imm : int;
  opcode : int;
}
type j_type = {
  rd : reg_type;
  imm : int;
  opcode : int;
}

type ops_type =
  | File of string
  | Glob of string
  | Local of string
  | Section of sec_type
  | String of string
and sec_type =
  | Text
  | Data
  | Rodata
  | Bss

type instr_type =
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

type t = (ops_type list * instr_type list) list

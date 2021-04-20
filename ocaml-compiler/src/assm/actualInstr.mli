type reg_type = Rv32i.reg_type

type branch_type = { lhs : reg_type; rhs : reg_type; offset : string; }
type load_type = { base : reg_type; dst : reg_type; offset : int; }
type store_type = { base : reg_type; src : reg_type; offset : int; }
type op_imm_type = { lhs : reg_type; rhs : int; dst : reg_type; }
type op_type = { lhs : reg_type; rhs : reg_type; dst : reg_type; }

type t = 
  | LUI   of { dst : reg_type; imm: int; }
  | AUIPC of { dst : reg_type; imm : int; }
  | JAL   of { dst : reg_type; offset : int; }
  | JALR  of { base : reg_type; dst : reg_type; offset : int; }
  | BEQ   of branch_type
  | BNE   of branch_type
  | BLT   of branch_type
  | BGE   of branch_type
  | BLTU  of branch_type
  | BGEU  of branch_type
  | LB    of load_type
  | LH    of load_type
  | LW    of load_type
  | LBU   of load_type
  | LHU   of load_type
  | SB    of store_type
  | SH    of store_type
  | SW    of store_type
  | ADDI  of op_imm_type
  | SLTI  of op_imm_type
  | SLTIU of op_imm_type
  | XORI  of op_imm_type
  | ORI   of op_imm_type
  | ANDI  of op_imm_type
  | SLLI  of op_imm_type
  | SRLI  of op_imm_type
  | SRAI  of op_imm_type
  | ADD   of op_type
  | SUB   of op_type
  | SLL   of op_type
  | SLT   of op_type
  | SLTU  of op_type
  | XOR   of op_type
  | SRL   of op_type
  | SRA   of op_type
  | OR    of op_type
  | AND   of op_type

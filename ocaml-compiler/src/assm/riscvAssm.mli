type reg_type = RiscvInstr.reg_type

(* https://github.com/riscv/riscv-asm-manual/blob/master/riscv-asm.md *)

type t = line_type list
and line_type =
  | Instr of assm_instr_type
  | Ops of RiscvInstr.ops_type
  | Label of string
and pseudoinstr_type =
  | LA of { rd : reg_type; symbol : string; }  (* Load address *)
  | LG of { rd : reg_type; symbol : string; }  (* Load global *)
  | SG of { rd : reg_type; rt : reg_type; symbol : string; }  (* Store global *)
  | NOP
  | LI of { rd : reg_type; imm : int }
  | MV of { rd : reg_type; rs : reg_type }
  | NOT of { rd : reg_type; rs : reg_type }
  | NEG of { rd : reg_type; rs : reg_type }
  | SEQZ of { rd : reg_type; rs : reg_type }  (* rd <- 1 if rs = zero *)
  | SNEZ of { rd : reg_type; rs : reg_type }
  | SLTZ of { rd : reg_type; rs : reg_type }
  | SGTZ of { rd : reg_type; rs : reg_type }
  | BEQZ of { rs : reg_type; offset : string }  (* beq rs,zero,offset *)
  | BNEZ of { rs : reg_type; offset : string }  (* bne rs,zero,offset *)
  | BLEZ of { rs : reg_type; offset : string }  (* ble rs,zero,offset *)
  | BGEZ of { rs : reg_type; offset : string }  (* bge rs,zero,offset *)
  | BLTZ of { rs : reg_type; offset : string }
  | BGTZ of { rs : reg_type; offset : string }
  | BGT of { rs : reg_type; rt : reg_type; offset : string }
  | BLE of { rs : reg_type; rt : reg_type; offset : string }
  | J of { offset : string }   (* jal zero,offset *)
  | JAL of { offset : string } (* jal ra,offset   *)
  | JR of { rs : reg_type; }   (* jalr zero,rs,0  *)
  | JALR of { rs : reg_type }  (* jalr ra,rs,0    *)
  | RET
  | CALL of { offset : string }
  | TAIL of { offset : string }
and actualinstr_type = 
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
and assm_instr_type =
  | Pseudo of pseudoinstr_type
  | Actual of actualinstr_type
and branch_type = { lhs : reg_type; rhs : reg_type; offset : string; }
and load_type = { base : reg_type; dst : reg_type; offset : int; }
and store_type = { base : reg_type; src : reg_type; offset : int; }
and op_imm_type = { lhs : reg_type; rhs : int; dst : reg_type; }
and op_type = { lhs : reg_type; rhs : reg_type; dst : reg_type; }

type reg_type = Rv32i.reg_type

(* https://github.com/riscv/riscv-asm-manual/blob/master/riscv-asm.md *)
type t =
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
  | CALLR of { rs : reg_type }  (* This is not standard *)
  | TAILR of { rs : reg_type }  (* This is not standard *)

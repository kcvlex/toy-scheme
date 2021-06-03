module INSTR_TYPE(
    input  wire [4:0] opcode,
    output wire [2:0] instr_format,
    output wire [4:0] instr_type
);
    `include "instr_type.hv"

    assign #1 { instr_format, instr_type } = opcode == 5'b00_000 ? { I_TYPE, LOAD }
                                           : opcode == 5'b01_000 ? { S_TYPE, STORE }
                                           : opcode == 5'b10_000 ? { R4_TYPE, MADD }
                                           : opcode[2:0] == 3'b000 ? { B_TYPE, BRANCH }
                                           : opcode == 5'b11_001 ? { I_TYPE, JALR }
                                           : opcode[2:0] == 3'b001 ? { I_TYPE, JALR }   // FIXME
                                           : opcode == 5'b10_010 ? { I_TYPE, NMSUB }
                                           : opcode[2:0] == 3'b010 ? { I_TYPE, NMSUB }  // FIXME
                                           : opcode == 5'b11_011 ? { J_TYPE, JAL }
                                           : opcode[2:0] == 3'b011 ? { J_TYPE, JAL }    // FIXME
                                           : opcode == 5'b00_100 ? { I_TYPE, OP_IMM }
                                           : opcode == 5'b01_100 ? { R_TYPE, OP }
                                           : opcode[2:0] == 3'b100 ? { R_TYPE, OP }     // FIXME
                                           : opcode == 5'b00_101 ? { U_TYPE, AUIPC }
                                           : opcode == 5'b01_101 ? { U_TYPE, LUI }
                                           : opcode[2:0] == 3'b101 ? { U_TYPE, LUI }    // FIXME
                                           : opcode == 5'b00_110 ? { I_TYPE, OP_IMM_32 }
                                           : opcode == 5'b01_110 ? { R_TYPE, OP_32 }
                                           : opcode[2:0] == 3'b110 ? { R_TYPE, OP_32 }  // FIXME
                                           :                       8'b0;
endmodule

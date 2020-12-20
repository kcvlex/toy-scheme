module EXTEND_IMM(
    input wire [31:0] instr,
    input wire [2:0] instr_type,
    output wire [31:0] imm
);
    `include "instr_type.hv"

    wire [11:0] #5 imm12 = (instr_type == I_TYPE ? instr[31:20] :
                            instr_type == S_TYPE ? { instr[31:25], instr[11:7] } : 0);
    wire [12:0] #5 imm13 = (instr_type == B_TYPE ? { instr[31], instr[7], instr[30:25], instr[11:8], 1'b0 } : 0);
    wire [19:0] #5 imm20 = (instr_type == J_TYPE ? { instr[31], instr[19:12], instr[20], instr[30:21], 1'b0 } : 0);

    assign imm = (instr_type == R_TYPE ? 32'd0 :
                  instr_type == I_TYPE ? { {20{imm12[11]}}, imm12[11:0] } :
                  instr_type == S_TYPE ? { {20{imm12[11]}}, imm12[11:0] } :
                  instr_type == B_TYPE ? { {19{imm13[12]}}, imm13[12:0] } :
                  instr_type == U_TYPE ? { instr[31:12], 12'd0 } :
                  instr_type == J_TYPE ? { {12{imm20[19]}}, imm20[19:0] } :
                                         32'd0);
endmodule

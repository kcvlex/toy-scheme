module ALU(
    input wire CLK, RST_X,
    input wire [31:0] rrs1, rrs2, imm,
    input wire [4:0] instr_type,
    input wire [2:0] funct3,
    input wire [6:0] funct7,
    output wire [31:0] w_res
);
    `include "instr_type.hv"
    `include "alu.hv"
    
    reg [31:0] r_res;
    wire signed [31:0] s_imm = $signed(imm);
    wire signed [31:0] s_rrs1 = $signed(rrs1);
    wire signed [31:0] s_rrs2 = $signed(rrs2);

    always @(*) begin
        if (!RST_X) begin
            r_res <= #5 0;
        end else begin
            case ({ instr_type, funct3 })
                { OP_IMM, FUNCT3_ADDI  }: r_res <= #5 rrs1 + imm;
                { OP_IMM, FUNCT3_SLTI  }: r_res <= #5 { 31'b0, (s_rrs1 < s_imm ? 1'b1 : 1'b0) };
                { OP_IMM, FUNCT3_SLTIU }: r_res <= #5 { 31'b0, (rrs1 < imm ? 1'b1 : 1'b0) };
                { OP_IMM, FUNCT3_XORI  }: r_res <= #5 rrs1 ^ imm;
                { OP_IMM, FUNCT3_ORI   }: r_res <= #5 rrs1 | imm;
                { OP_IMM, FUNCT3_ANDI  }: r_res <= #5 rrs1 & imm;
                { OP_IMM, FUNCT3_SLLI  }: r_res <= #5 rrs1 << imm;
                { OP_IMM, FUNCT3_SRI   }: r_res <= #5 (funct7 == FUNCT7_SRLI ? (rrs1 >> imm) :
                                                       funct7 == FUNCT7_SRAI ? (s_rrs1 >>> s_imm) :
                                                                              0);
                { OP, FUNCT3_ADD }: r_res <= #5 (funct7 == FUNCT7_ADD ? rrs1 + rrs2 :
                                                 funct7 == FUNCT7_SUB ? rrs1 - rrs2 :
                                                                        0);
                { OP, FUNCT3_SLL  }: r_res <= #5 rrs1 << rrs2;
                { OP, FUNCT3_SLT  }: r_res <= #5 { 31'b0, (s_rrs1 < s_rrs2 ? 1'b1 : 1'b0) };
                { OP, FUNCT3_SLTU }: r_res <= #5 { 31'b0, (rrs1 < rrs2 ? 1'b1 : 1'b0) };
                { OP, FUNCT3_XOR  }: r_res <= #5 rrs1 ^ rrs2;
                { OP, FUNCT3_SR   }: r_res <= #5 (funct7 == FUNCT7_SRL ? rrs1 >> rrs2[4:0] :
                                                  funct7 == FUNCT7_SRA ? s_rrs1 >> rrs2 :
                                                                         0);
                { OP, FUNCT3_OR   }: r_res <= #5 rrs1 | rrs2;
                { OP, FUNCT3_AND  }: r_res <= #5 rrs1 & rrs2;
                default: r_res <= #5 0;
            endcase
        end
    end

    assign #5 w_res = r_res;

endmodule

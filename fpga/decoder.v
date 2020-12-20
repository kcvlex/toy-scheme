module INSTR_DECODER(
    input wire [31:0] instr,
    output wire [2:0] instr_type,
    output wire [4:0] rs1, rs2, rd,
    output wire [31:0] imm,
    output wire [2:0] funct3,
    output wire [6:0] funct7,
    output wire [4:0] opcode
);
    assign opcode = instr[6:2];
    assign rs1 = instr[19:15];
    assign rs2 = instr[24:20];
    assign rd = instr[11:7];
    assign funct3 = instr[14:12];
    assign funct7 = instr[31:25];

    INSTR_TYPE instr_t(opcode, instr_type);
    EXTEND_IMM extend_imm(instr, instr_type, imm);
endmodule

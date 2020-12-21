module CPU(
    input wire CLK, RST_X,
    input wire [31:0] PC,
    input wire [31:0] instr
);
    `include "instr_type.hv"

    wire [4:0] rs1, rs2, rd;
    wire [31:0] rrs1, rrs2, imm, a0;
    wire [2:0] funct3;
    wire [6:0] funct7;
    wire [4:0] instr_type;
    wire [2:0] instr_format;
    wire [31:0] result;

    INSTR_DECODER decoder(
        .instr(instr),
        .rs1(rs1),
        .rs2(rs2),
        .rd(rd),
        .imm(imm),
        .funct3(funct3),
        .funct7(funct7),
        .instr_type(instr_type),
        .instr_format(instr_format)
    );

    REGFILE regfile(
        .CLK(CLK),
        .RST_X(RST_X),
        .we(we),
        .rs1(rs1),
        .rs2(rs2),
        .rd(rd),
        .wd(result),
        .rrs1(rrs1),
        .rrs2(rrs2),
        .a0(a0)
    );

    assign #5 we = ((instr_format == R_TYPE ||
                     instr_format == I_TYPE ||
                     instr_format == U_TYPE ||
                     instr_format == J_TYPE) ? 1'b1 : 1'b0);

    ALU alu(
        .CLK(CLK),
        .RST_X(RST_X),
        .rrs1(rrs1),
        .rrs2(rrs2),
        .imm(imm),
        .instr_type(instr_type),
        .funct3(funct3),
        .funct7(funct7),
        .w_res(result)
    );

endmodule

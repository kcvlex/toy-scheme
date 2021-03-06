module INSTR_DECODER(
    input  wire clk,
    input  wire reset,
    input  wire req,
    input  wire [31:0] instr,
    output wire [4:0] rs1, rs2, rd,
    output wire [31:0] imm,
    output wire [2:0] funct3,
    output wire [6:0] funct7,
    output wire [4:0] instr_type
);
    assign #1 funct7 = instr[31:25];
    assign #1 rs2    = instr[24:20];
    assign #1 rs1    = instr[19:15];
    assign #1 funct3 = instr[14:12];
    assign #1 rd     = instr[11:7];
   
    wire [4:0] #1 opcode = instr[6:2];
    wire [2:0] instr_format;
 
    INSTR_TYPE itype(
        .opcode(opcode),
        .instr_format(instr_format),
        .instr_type(instr_type)
    );
    
    EXTEND_IMM extend_imm(
        .instr(instr), 
        .instr_format(instr_format), 
        .imm(imm)
    );
endmodule

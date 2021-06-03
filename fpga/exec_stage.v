module EXEC_STAGE(
    input wire clk,
    input wire reset,

    ///// REQ /////
    input wire req,

    ///// INPUT /////
    input wire [31:0] pc,
    input wire [31:0] rrs1,
    input wire [31:0] rrs2,
    input wire [31:0] imm,
    input wire [4:0]  instr_type,
    input wire [2:0]  funct3,
    input wire [6:0]  funct7,

    ///// OUTPUT /////
    output wire [31:0] next_pc,
    output wire [31:0] result,
    output wire [31:0] wr_mem_addr,
    output wire [31:0] rd_mem_addr,
    output wire [31:0] wr_mem_data,
    output wire        rd_mem,
    output wire        wr_mem,
    output wire        wr_regfile
);
    `include "instr_type.hv"
    `include "funct.hv"
    `include "alu.hv"

    wire [31:0] arith_lhs, arith_rhs, logic_lhs, logic_rhs;
    wire [4:0]  arith_op, logic_op;
    wire        jump;
    wire [31:0] arith_res, logic_res;
    wire        is_lhs_pc;
    
    // Arithmetic
    assign #1 is_lhs_pc = (instr_type == BRANCH |
                           instr_type == JAL    |
                           instr_type == AUIPC);
    assign #1 arith_lhs = (is_lhs_pc         ? pc
                        :  instr_type == LUI ? 32'b0 
                        :  rrs1);
    assign #1 arith_rhs = (instr_type == OP ? rrs2 : imm);
    assign #1 arith_op  = (instr_type != OP && instr_type != OP_IMM     ? ADD
                        :  funct3 == FUNCT3_ADD && instr_type == OP_IMM ? ADD
                        :  funct3 == FUNCT3_ADD && funct7 == FUNCT7_ADD ? ADD
                        :  funct3 == FUNCT3_ADD                         ? SUB
                        :  funct3 == FUNCT3_SLL                         ? SLL
                        :  funct3 == FUNCT3_SLT                         ? SLT
                        :  funct3 == FUNCT3_SLTU                        ? SLTU
                        :  funct3 == FUNCT3_XOR                         ? XOR
                        :  funct3 == FUNCT3_SR && funct7 == FUNCT7_SRL  ? SRL
                        :  funct3 == FUNCT3_SR                          ? SRA
                        :  funct3 == FUNCT3_OR                          ? OR
                        :  funct3 == FUNCT3_AND                         ? AND
                        :                                                 5'b0);

    // Logic
    assign #1 logic_lhs = (instr_type == BRANCH ? rrs1 : 32'b0);
    assign #1 logic_rhs = (instr_type == BRANCH ? rrs2 : 32'b0);
    assign #1 logic_op  = (funct3 == FUNCT3_EQ  ? EQ
                        :  funct3 == FUNCT3_NEQ ? NEQ
                        :  funct3 == FUNCT3_LT  ? LT
                        :  funct3 == FUNCT3_GE  ? GE
                        :  funct3 == FUNCT3_LTU ? LTU
                        :  funct3 == FUNCT3_GEU ? GEU
                        :                         5'b0);


    // Jump
    assign #1 jump = (instr_type == BRANCH |
                      instr_type == JAL    |
                      instr_type == JALR);

        
    ALU arith_alu(
        .clk(clk),
        .reset(reset),
        .req(req),
        .lhs(arith_lhs),
        .rhs(arith_rhs),
        .op(arith_op),
        .res(arith_res)
    );

    ALU logic_alu(
        .clk(clk),
        .reset(reset),
        .req(req),
        .lhs(logic_lhs),
        .rhs(logic_rhs),
        .op(logic_op),
        .res(logic_res)
    );

    // Next PC
    assign #1 next_pc    = (~jump                ? pc + 4
                         :  instr_type != BRANCH ? arith_res
                         :  logic_res            ? arith_res
                         :  pc + 4);

    // Result
    assign #1 result = (jump ? pc + 4 : arith_res);

    // MEM
    assign #1 wr_mem_data = rrs2;
    assign #1 wr_mem      = (instr_type == STORE);
    assign #1 rd_mem      = (instr_type == LOAD);
    assign #1 wr_mem_addr = (wr_mem ? arith_res : 32'b0);
    assign #1 rd_mem_addr = (rd_mem ? arith_res : 32'b0);

    // WB
    assign #1 wr_regfile  = (instr_type != STORE && instr_type != BRANCH);
endmodule

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

    reg [31:0]  r_arith_lhs = 32'b0, 
                r_arith_rhs = 32'b0,
                r_logic_lhs = 32'b0,
                r_logic_rhs = 32'b0;
    reg [4:0]   r_arith_op  = 4'b0,
                r_logic_op  = 4'b0;
    reg         r_jump      = 1'b0;
    wire [31:0] arith_res, 
                logic_res;
    
    always @(*) if (req) begin
        r_logic_lhs  <= #1 (instr_type == BRANCH ? rrs1 : 32'b0);
        r_logic_rhs  <= #1 (instr_type == BRANCH ? rrs2 : 32'b0);

        case (instr_type)
            BRANCH, JAL, JALR: r_jump <= #1 1;
            default:           r_jump <= #1 0;
        endcase
        
        case (instr_type)
            OP, OP_IMM, STORE, LOAD, JALR: r_arith_lhs <= #1 rrs1;
            BRANCH, JAL, AUIPC:            r_arith_lhs <= #1 pc;
            LUI:                           r_arith_lhs <= #1 0;
        endcase
        
        case (instr_type)
            OP:      r_arith_rhs <= #1 rrs2;
            default: r_arith_rhs <= #1 imm;
        endcase

        if (instr_type == OP || instr_type == OP_IMM) begin
            case (funct3)
                FUNCT3_ADD:  r_arith_op <= #1 (instr_type == OP_IMM ? ADD :
                                               funct7 == FUNCT7_ADD ? ADD : SUB);  // FIXME
                FUNCT3_SLL:  r_arith_op <= #1 SLL;
                FUNCT3_SLT:  r_arith_op <= #1 SLT;
                FUNCT3_SLTU: r_arith_op <= #1 SLTU;
                FUNCT3_XOR:  r_arith_op <= #1 XOR;
                FUNCT3_SR:   r_arith_op <= #1 (funct7 == FUNCT7_SRL ? SRL : SRA);  // FIXME
                FUNCT3_OR:   r_arith_op <= #1 OR;
                FUNCT3_AND:  r_arith_op <= #1 AND;
                default:     r_arith_op <= #1 0;
            endcase
        end else begin
            r_arith_op <= #1 ADD;
        end
        
        case (funct3)
            FUNCT3_EQ:   r_logic_op <= #1 EQ;
            FUNCT3_NEQ:  r_logic_op <= #1 NEQ;
            FUNCT3_LT:   r_logic_op <= #1 LT;
            FUNCT3_GE:   r_logic_op <= #1 GE;
            FUNCT3_LTU:  r_logic_op <= #1 LTU;
            FUNCT3_GEU:  r_logic_op <= #1 GEU;
            default:     r_logic_op <= #1 0;
        endcase
    end

    ALU arith_alu(
        .clk(clk),
        .reset(reset),
        .lhs(r_arith_lhs),
        .rhs(r_arith_rhs),
        .op(r_arith_op),
        .res(arith_res)
    );

    ALU logic_alu(
        .clk(clk),
        .reset(reset),
        .lhs(r_logic_lhs),
        .rhs(r_logic_rhs),
        .op(r_logic_op),
        .res(logic_res)
    );
    
    assign #1 next_pc    = (~r_jump              ? pc + 4    :
                            instr_type != BRANCH ? arith_res :
                            logic_res            ? arith_res : pc + 4);
    assign #1 result      = (r_jump ? pc + 4 : arith_res);
    assign #1 wr_mem_data = rrs2;
    assign #1 wr_mem      = (instr_type == STORE);
    assign #1 rd_mem      = (instr_type == LOAD);
    assign #1 wr_mem_addr = (wr_mem ? arith_res : 32'b0);
    assign #1 rd_mem_addr = (rd_mem ? arith_res : 32'b0);
    assign #1 wr_regfile  = (instr_type != STORE && instr_type != BRANCH);
endmodule

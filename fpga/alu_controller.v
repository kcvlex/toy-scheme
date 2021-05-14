`include "params.hv"

module ALU_CONTROLLER #(
    parameter AWIDTH = 25
) (
    input wire CLK, RST_X,
    input wire [31:0] pc,
    input wire [31:0] rrs1, rrs2, imm,
    input wire [4:0] instr_type,
    input wire [2:0] funct3,
    input wire [6:0] funct7,

    output wire        wr_regfile,
    output wire [31:0] wr_regfile_data,
    output wire [31:0] next_pc,
	
    output wire [AWIDTH-1:0] sdram_addr,
    output wire [31:0] sdram_wr_data,
    output wire sdram_wr_req,
    output wire sdram_rd_req
);
    `include "instr_type.hv"
    `include "alu.hv"
    `include "funct.hv"

    reg [31:0] r_arith_lhs, r_arith_rhs;
    reg [31:0] r_logic_lhs, r_logic_rhs;
    reg [4:0]  r_arith_op, r_logic_op;
    reg        r_jump;
    
    wire [31:0] arith_res, logic_res;

    always @(*) begin
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
        .CLK(CLK),
        .RST_X(RST_X),
        .lhs(r_arith_lhs),
        .rhs(r_arith_rhs),
        .op(r_arith_op),
        .res(arith_res)
    );

    ALU logic_alu(
        .CLK(CLK),
        .RST_X(RST_X),
        .lhs(r_logic_lhs),
        .rhs(r_logic_rhs),
        .op(r_logic_op),
        .res(logic_res)
    );

    INCR_PC incr_pc(
        .pc(pc),
        .instr_type(instr_type),
        .jump(r_jump),
        .arith_res(arith_res),
        .logic_res(logic_res),
        .next_pc(next_pc)
    );

    assign #1 sdram_rd_req    = (instr_type == LOAD);
    assign #1 sdram_wr_req    = (instr_type == STORE);
    assign #1 sdram_wr_data   = (instr_type == STORE ? rrs1 : 32'b0);
    assign #1 sdram_addr      = (sdram_rd_req || sdram_wr_req ? arith_res[AWIDTH-1:0] : 0);
    assign #1 wr_regfile      = (instr_type != STORE && instr_type != BRANCH);
    assign #1 wr_regfile_data = (r_jump ? pc + 4 : arith_res);
endmodule

module INCR_PC(
    input wire [31:0] pc,
    input wire [4:0] instr_type,
    input wire jump,
    input wire [31:0] arith_res, logic_res,
    output wire [31:0] next_pc
);
    `include "instr_type.hv"
    assign #1 next_pc = (!jump                ? pc + 4 :
                         instr_type != BRANCH ? arith_res :
                         logic_res            ? arith_res
                                              : pc + 4);
endmodule

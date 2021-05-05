module ALU_CONTROLLER(
    input wire CLK, RST_X,
    input wire valid_instr,
    input wire [31:0] pc,
    input wire [31:0] rrs1, rrs2, imm,
    input wire [4:0] instr_type,
    input wire [2:0] funct3,
    input wire [6:0] funct7,
    input wire [4:0] rd_in,
    output wire store, load, we_reg,
    output wire [31:0] wd_mem, ls_mem_addr,
    output wire [31:0] wd_reg,
    output wire [31:0] next_pc,
    output wire halt,
    output wire valid,
    output wire [4:0] rd_out
);    
    `include "instr_type.hv"
    `include "alu.hv"
    `include "funct.hv"

    reg [31:0] r_arith_lhs, r_arith_rhs, r_logic_lhs, r_logic_rhs;
    reg [4:0] r_arith, r_logic;
    reg r_store, r_load, r_we_reg, r_halt, r_jump;
    wire [31:0] arith_res, logic_res;

    always @(*) r_store <= #1 instr_type == STORE;

    always @(*) r_load  <= #1 instr_type == LOAD;

    always @(*) r_we_reg <= #1 (instr_type != STORE && instr_type != BRANCH);

    always @(*) r_halt <= #1 0;

    always @(*) begin
        case (instr_type)
            BRANCH, JAL, JALR: r_jump <= #1 1;
            default:           r_jump <= #1 0;
        endcase
    end

    always @(*) begin
        case (instr_type)
            OP, OP_IMM, STORE, LOAD, JALR: r_arith_lhs <= #1 rrs1;
            BRANCH, JAL, AUIPC:            r_arith_lhs <= #1 pc;
            LUI:                           r_arith_lhs <= #1 0;
        endcase
    end
    
    always @(*) begin
        case (instr_type)
            OP:      r_arith_rhs <= #1 rrs2;
            default: r_arith_rhs <= #1 imm;
        endcase
    end

    always @(*) r_logic_lhs <= #1 (instr_type == BRANCH ? rrs1 : 32'b0);
    
    always @(*) r_logic_rhs <= #1 (instr_type == BRANCH ? rrs2 : 32'b0);

    always @(*) begin
        if (instr_type == OP || instr_type == OP_IMM) begin
            case (funct3)
                FUNCT3_ADD:  r_arith <= #1 (instr_type == OP_IMM ? ADD :
                                            funct7 == FUNCT7_ADD ? ADD : SUB);  // FIXME
                FUNCT3_SLL:  r_arith <= #1 SLL;
                FUNCT3_SLT:  r_arith <= #1 SLT;
                FUNCT3_SLTU: r_arith <= #1 SLTU;
                FUNCT3_XOR:  r_arith <= #1 XOR;
                FUNCT3_SR:   r_arith <= #1 (funct7 == FUNCT7_SRL ? SRL : SRA);  // FIXME
                FUNCT3_OR:   r_arith <= #1 OR;
                FUNCT3_AND:  r_arith <= #1 AND;
                default:     r_arith <= #1 0;
            endcase
        end else begin
            r_arith <= #1 ADD;
        end
    end
    
    always @(*) begin
        if (instr_type == BRANCH) begin
            case (funct3)
                FUNCT3_EQ:   r_logic <= #1 EQ;
                FUNCT3_NEQ:  r_logic <= #1 NEQ;
                FUNCT3_LT:   r_logic <= #1 LT;
                FUNCT3_GE:   r_logic <= #1 GE;
                FUNCT3_LTU:  r_logic <= #1 LTU;
                FUNCT3_GEU:  r_logic <= #1 GEU;
                default:     r_logic <= #1 0;
            endcase
        end else begin
            r_logic <= #1 0;
        end
    end

    ALU arith_alu(
        .CLK(CLK),
        .RST_X(RST_X),
        .lhs(r_arith_lhs),
        .rhs(r_arith_rhs),
        .op(r_arith),
        .res(arith_res)
    );

    ALU logic_alu(
        .CLK(CLK),
        .RST_X(RST_X),
        .lhs(r_logic_lhs),
        .rhs(r_logic_rhs),
        .op(r_logic),
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

    assign #1 store       = ((RST_X && r_store) ? 1 : 0);
    assign #1 load        = ((RST_X && r_load) ? 1 : 0);
    assign #1 we_reg      = ((RST_X && r_we_reg) ? 1 : 0);
    assign #1 wd_reg      = ((RST_X && (instr_type == JAL || instr_type == JALR)) ? pc + 4 :
                             (RST_X && r_we_reg && !r_load)                       ? arith_res : 0);
    assign #1 halt        = ((RST_X && r_halt) ? 1 : 0);
    assign #1 wd_mem      = ((RST_X && r_store) ? rrs2 : 0);
    assign #1 ls_mem_addr = ((RST_X && (r_store || r_load)) ? arith_res : 0);
    assign #1 valid       = ((RST_X && valid_instr) ? 1 : 0);
    assign #1 rd_out      = (RST_X ? rd_in : 0);
endmodule

module INCR_PC(
    input wire [31:0] pc,
    input wire [4:0] instr_type,
    input wire jump,
    input wire [31:0] arith_res, logic_res,
    output wire [31:0] next_pc
);
    `include "instr_type.hv"
    reg [31:0] r_next_pc;

    always @(*) begin
        if (!jump) begin
            r_next_pc <= #1 pc + 4;
        end else if (instr_type != BRANCH) begin
            r_next_pc <= #1 arith_res;
        end else begin
            r_next_pc <= #1 (logic_res ? arith_res : pc + 4);
        end
    end

    assign #1 next_pc = r_next_pc;
endmodule

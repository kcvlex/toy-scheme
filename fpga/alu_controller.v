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

    reg [31:0] r_lhs, r_rhs;
    reg [4:0] r_op;
    reg r_store, r_load, r_we_reg, r_halt, r_jump;
    wire [31:0] alu_res;

    always @(*) begin
        if (!RST_X) begin
            r_store  <= #1 0;
            r_load   <= #1 0;
            r_we_reg <= #1 0;
            r_halt   <= #1 0;
            r_jump   <= #1 0;
        end else begin
            r_store   <= #1 (instr_type == STORE ? 1 : 0);
            r_load    <= #1 (instr_type == LOAD  ? 1 : 0);
            
            // FIXME branch
            // operand
            case (instr_type)
                OP, OP_IMM: begin
                    r_lhs     <= #1 rrs1;
                    r_rhs     <= #1 (instr_type == OP ? rrs2 : imm);
                    r_we_reg  <= #1 1;
                    r_halt    <= #1 0;
                    r_jump    <= #1 0;
                    
                    // operator
                    case (funct3) 
                        FUNCT3_ADD:  r_op <= #1 (instr_type == OP_IMM ? ADD :
                                                 funct7 == FUNCT7_ADD ? ADD : SUB);  // FIXME
                        FUNCT3_SLL:  r_op <= #1 SLL;
                        FUNCT3_SLT:  r_op <= #1 SLT;
                        FUNCT3_SLTU: r_op <= #1 SLTU;
                        FUNCT3_XOR:  r_op <= #1 XOR;
                        FUNCT3_SR:   r_op <= #1 (funct7 == FUNCT7_SRL ? SRL : SRA);  // FIXME
                        FUNCT3_OR:   r_op <= #1 OR;
                        FUNCT3_AND:  r_op <= #1 AND;
                        default: r_op <= #1 0;
                    endcase
                end
                STORE: begin
                    r_lhs     <= #1 rrs1;
                    r_rhs     <= #1 imm;
                    r_we_reg  <= #1 0;
                    r_halt    <= #1 0;
                    r_op      <= #1 ADD;
                    r_jump    <= #1 0;
                end
                LOAD: begin
                    r_lhs     <= #1 rrs1;
                    r_rhs     <= #1 imm;
                    r_we_reg  <= #1 1;
                    r_halt    <= #1 0;
                    r_op      <= #1 ADD;
                    r_jump    <= #1 0;
                end
                JAL: begin
                    r_lhs     <= #1 pc;
                    r_rhs     <= #1 imm;
                    r_we_reg  <= #1 1;
                    r_halt    <= #1 0;
                    r_op      <= #1 ADD;
                    r_jump    <= #1 1;
                end
                JALR: begin
                    r_lhs     <= #1 rrs1;
                    r_rhs     <= #1 imm;
                    r_we_reg  <= #1 1;
                    r_halt    <= #1 0;
                    r_op      <= #1 ADD;
                    r_jump    <= #1 1;
                end
            endcase
        end
    end

    ALU alu(
        .CLK(CLK),
        .RST_X(RST_X),
        .lhs(r_lhs),
        .rhs(r_rhs),
        .op(r_op),
        .res(alu_res)
    );

    assign #1 store       = ((RST_X && r_store) ? 1 : 0);
    assign #1 load        = ((RST_X && r_load) ? 1 : 0);
    assign #1 we_reg      = ((RST_X && r_we_reg) ? 1 : 0);
    assign #1 wd_reg      = ((RST_X && (instr_type == JAL || instr_type == JALR)) ? pc + 4 :
                             (RST_X && r_we_reg && !r_load)                       ? alu_res : 0);
    assign #1 halt        = ((RST_X && r_halt) ? 1 : 0);
    assign #1 next_pc     = ((RST_X && !r_halt) ? (r_jump ? alu_res : pc + 4) : pc);
    assign #1 wd_mem      = ((RST_X && r_store) ? rrs2 : 0);
    assign #1 ls_mem_addr = ((RST_X && (r_store || r_load)) ? alu_res : 0);
    assign #1 valid       = ((RST_X && valid_instr) ? 1 : 0);
    assign #1 rd_out      = (RST_X ? rd_in : 0);

endmodule

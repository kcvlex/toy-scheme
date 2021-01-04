module CPU(
    input wire CLK, RST_X,
    input wire [31:0] pc,
    input wire [31:0] instr,
    output wire [31:0] next_pc
);
    `include "instr_type.hv"

    /* Instruction Decode */
    wire [4:0] rs1, rs2, rd;
    wire [31:0] rrs1, rrs2, imm;
    wire [2:0] funct3;
    wire [6:0] funct7;
    wire [4:0] instr_type;
    wire [2:0] instr_format;
    wire Id_valid;

    /* Exec */
    wire Ex_store, Ex_load;
    wire [31:0] Ex_wd_reg, Ex_wd_mem, Ex_ls_mem_addr;
    wire [4:0] Ex_rd;
    wire Ex_we_reg, Ex_valid;

    /* WB */
    wire [31:0] Wb_wd_reg;
    wire [4:0] Wb_rd;
    wire Wb_valid;

    wire we_reg;
    wire [31:0] wd_reg;

    INSTR_DECODER decoder(
        .RST_X(RST_X),
        .instr(instr),
        .rs1(rs1),
        .rs2(rs2),
        .rd(rd),
        .imm(imm),
        .funct3(funct3),
        .funct7(funct7),
        .instr_type(instr_type),
        .instr_format(instr_format),
        .valid(Id_valid)
    );

    REGFILE regfile(
        .CLK(CLK),
        .RST_X(RST_X),
        .we(we_reg),
        .rs1(rs1),
        .rs2(rs2),
        .rd(rd),
        .wd(wd_reg),
        .rrs1(rrs1),
        .rrs2(rrs2)
    );

    /*
    assign #1 we = ((instr_format == R_TYPE ||
                     instr_format == I_TYPE ||
                     instr_format == U_TYPE ||
                     instr_format == J_TYPE) ? 1'b1 : 1'b0);
    */

    ALU_CONTROLLER alu_cont(
        .CLK(CLK),
        .RST_X(RST_X),
        .valid_instr(Id_valid),
        .pc(pc),
        .rrs1(rrs1),
        .rrs2(rrs2),
        .imm(imm),
        .instr_type(instr_type),
        .funct3(funct3),
        .funct7(funct7),
        .store(Ex_store),
        .load(Ex_load),
        .we_reg(Ex_we_reg),
        .halt(halt),
        .wd_mem(Ex_wd_mem),
        .ls_mem_addr(Ex_ls_mem_addr),
        .wd_reg(Ex_wd_reg),
        .next_pc(next_pc),
        .valid(Ex_valid)
    );

    SIMPLE_SDRAM sdram(
        .CLK(CLK),
        .RST_X(RST_X),
        .addr(Ex_ls_mem_addr),
        .wd(Ex_wd_mem),
        .we(Ex_store),
        .valid_in(Ex_valid),
        .rd_in(Ex_rd),
        .loaded(Wb_wd_reg),
        .valid_out(Wb_valid),
        .rd_out(Wb_rd)
    );

    assign #1 we_reg = (Wb_valid && RST_X && Ex_we_reg);
    assign #1 wd_reg = (Ex_load ? Wb_wd_reg : Ex_wd_reg);
endmodule

module MUX(
    input wire cond,
    input wire [31:0] v1, v2,
    output wire v
);
    assign #1 v = (cond ? v1 : v2);
endmodule

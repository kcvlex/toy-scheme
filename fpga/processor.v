module PROCESSOR(
    input wire CLK,
    input wire RST_X
);
    reg [31:0] PC;
    wire [31:0] instr, next_pc;

    always @(posedge RST_X) PC <= #1 4;
    always @(posedge CLK) if (RST_X) PC <= #1 next_pc;

    IMEM imem(CLK, RST_X, PC, instr);
    CPU cpu(CLK, RST_X, PC, instr, next_pc);
endmodule

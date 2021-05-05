module REGFILE(
    input wire CLK, RST_X,
    input wire we,
    input wire [4:0] rs1, rs2, rd,
    input wire [31:0] wd,
    output wire [31:0] rrs1, rrs2
);
    reg [31:0] x[0:31];

    always @(posedge RST_X) begin
        x[0] <= #1 0;
        x[1] <= #1 0;  // return address(link register)
        x[2] <= #1 (32'hff00 - 1) << 2;  // stack pointer
        x[8] <= #1 (32'hff00 - 1) << 2;  // base pointer
    end

    always @(negedge CLK) if (RST_X && we && rd != 0) begin
        x[rd] <= #1 wd;
        // $write("x[%d] <= %x\n", rd, wd);
    end

    assign #1 rrs1 = (RST_X ? x[rs1] : 32'h0);
    assign #1 rrs2 = (RST_X ? x[rs2] : 32'h0);
endmodule

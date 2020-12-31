module REGFILE(
    input wire CLK, RST_X,
    input wire we,
    input wire [4:0] rs1, rs2, rd,
    input wire [31:0] wd,
    output wire [31:0] rrs1, rrs2
);
    reg [31:0] x[0:31];

    always @(posedge RST_X) begin
        x[0] <= #5 0;
        x[1] <= #5 0;  // return address(link register)
        x[2] <= #5 0;  // stack pointer
        x[8] <= #5 0;  // base pointer
    end

    always @(negedge CLK) if (RST_X && we && rd != 0) x[rd] <= #5 wd;

    assign #5 rrs1 = (RST_X ? x[rs1] : 32'h0);
    assign #5 rrs2 = (RST_X ? x[rs2] : 32'h0);
    // assign #5 a0 = (RST_X ? x[5'd10] : 32'h0);  // return value
endmodule

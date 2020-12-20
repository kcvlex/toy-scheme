module REGFILE(
    input wire CLK, RST_X,
    input wire WE,
    input wire [4:0] rs1, rs2, rd,
    input wire [31:0] wd,
    output wire [31:0] rrs1, rrs2, a0
);
    reg [31:0] x[0:31];
    always @(posedge RST_X) x[0] <= #5 0;
    always @(posedge CLK) begin
        if (RST_X && WE) begin
            x[rd] <= #5 wd;
        end
    end

    assign #5 rrs1 = (RST_X ? x[rs1] : 32'h0);
    assign #5 rrs2 = (RST_X ? x[rs2] : 32'h0);
    assign #5 a0 = (RST_X ? x[5'd10] : 32'h0);  // return value
endmodule

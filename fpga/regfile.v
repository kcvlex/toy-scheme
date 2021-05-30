module REGFILE(
    input  wire        clk, 
    input  wire        reset,
    input  wire        wr_req,
    input  wire [31:0] wr_data,
    input  wire [4:0]  rs1, 
    input  wire [4:0]  rs2,
    input  wire [4:0]  rd,
    output wire [31:0] rrs1, 
    output wire [31:0] rrs2
);
    reg [31:0] x[0:31];

    always @(negedge clk) begin
        if (reset) begin
            x[0] <= #1 0;
            x[1] <= #1 0;  // return address(link register)
            x[2] <= #1 32'hff00;  // stack pointer
            x[8] <= #1 32'hff00;  // base pointer
        end else begin
            if (wr_req && rd != 0) x[rd] <= #1 wr_data;
        end
    end

    assign #1 rrs1 = x[rs1];
    assign #1 rrs2 = x[rs2];
endmodule

module IMEM #(parameter WIDTH = 12) (
    input wire CLK, RST_X,
    input wire [31:0] addr,
    output wire [31:0] out_data
);
    reg [31:0] mem[0:(1 << WIDTH)-1];
    assign #1 out_data = (RST_X ? mem[addr[WIDTH+1:2]] : 0);
endmodule

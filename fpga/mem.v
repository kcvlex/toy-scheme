module MEM(
    input wire CLK, RST_X,
    input wire [31:0] addr,
    output wire [31:0] out_data
);
    reg [31:0] mem[0:1024];
    assign #5 out_data = (RST_X ? mem[addr[11:2]] : 0);
endmodule

module SIMPLE_SDRAM(
    input wire CLK, RST_X,
    input wire [31:0] addr,
    input wire we,
    input wire [31:0] waddr, wd,
    output wire [31:0] out_data
);
    reg [31:0] mem[0:65535];
    always @(posedge CLK) if (we && RST_X) mem[waddr[14:2]] <= #5 wd;
    assign out_data = mem[addr[14:2]];
endmodule

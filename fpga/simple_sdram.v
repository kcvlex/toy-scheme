module SIMPLE_SDRAM(
    input wire CLK, RST_X,
    input wire [31:0] addr, wd,
    input wire we,
    input wire [4:0] rd_in,
    input wire valid_in,
    output wire [31:0] loaded,
    output wire valid_out,
    output wire we_reg,
    output wire [4:0] rd_out
);
    reg [31:0] mem[0:65535];

    always @(negedge CLK) if (we && RST_X && valid_in) mem[addr[31:2]] <= #1 wd;

    assign #1 loaded = ((RST_X && valid_in) ? mem[addr[31:2]] : 0);
    assign #1 valid_out = ((RST_X && valid_in) ? 1 : 0);
    assign #1 rd_out = rd_in;
endmodule

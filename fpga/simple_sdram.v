module SIMPLE_SDRAM(
    input wire CLK, RST_X,
    input wire [31:0] addr, wd,
    input wire we,
    input wire [4:0] rd_in,
    input wire valid_in,
    input wire is_load,
    output wire [31:0] loaded,
    output wire valid_out,
    output wire we_reg,
    output wire [4:0] rd_out
);
    reg [31:0] mem[0:65535];

    always @(negedge CLK) begin
        if (we && RST_X && valid_in) begin
            mem[addr[31:2]] <= #1 wd;
            // $write("stored : %x\n", wd);
        end
        // if (is_load) $write("loaded : %x\n", mem[addr[31:2]]);
    end

    assign #1 loaded = ((RST_X && valid_in) ? mem[addr[31:2]] : 0);
    assign #1 valid_out = ((RST_X && valid_in) ? 1 : 0);
    assign #1 rd_out = rd_in;
endmodule

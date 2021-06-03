module CHECK_MEMORY(
    input wire clk,
    input wire reset,

    input wire sw,
    input wire key,

    input wire rd_fin,
    output reg rd_req,
    output reg [31:0] rd_addr,
    input wire [31:0] rd_data,
    input wire [31:0] rd_addr_init,
    output reg [31:0] dout
);
    wire key_pulse;
    DETECT_NEGEDGE detect_key(
        .CLK(clk),
        .RESET(reset),
        .SIGNAL(key),
        .EDGE(key_pulse),
    );

    always @(negedge clk or posedge reset) begin
        if (reset) begin
            rd_req <= 1'b0;
            rd_addr <= rd_addr_init;
            dout <= 32'b0;
        end else if (sw) begin
            if (~rd_req) begin
                if (key_pulse) begin
                    rd_req <= 1'b1;
                end
            end else begin
                if (rd_fin) begin
                    rd_req <= 1'b0;
                    rd_addr <= rd_addr + 32'd4;
                    dout <= rd_data;
                end
            end
        end
    end
endmodule

module SIMPLE_SDRAM #(
    parameter MEM_SIZE = 65536
) (
    input wire clk,
    input wire reset,

    input  wire        wr_req,
    output reg         wr_fin,
    input  wire [31:0] wr_addr,
    input  wire [31:0] wr_data,

    input  wire        rd_req,
    output reg         rd_fin,
    input  wire [31:0] rd_addr,
    output reg  [31:0] rd_data
);
    localparam IDLE   = 3'd0;
    localparam WR_REQ = 3'd1;
    localparam WR_FIN = 3'd2;
    localparam RD_REQ = 3'd3;
    localparam RD_FIN = 3'd4;

    reg [7:0] mem[MEM_SIZE-1:0];
    reg [2:0] state = IDLE;
    reg [31:0] addr;
    
    reg iclk = 1'b0;
    initial forever #20 iclk = ~iclk;

    always @(posedge iclk) begin
        if (reset) begin
            state   <= IDLE;
            rd_data <= 32'b0;
            rd_fin  <= 1'b0;
            wr_fin  <= 1'b0;
        end else begin
            case (state)
                IDLE: begin
                    if (wr_req) begin
                        state      <= WR_REQ;
                        wr_fin     <= 1'b0;
                        rd_fin     <= 1'b0;
                        addr       <= wr_addr;
                    end else if (rd_req) begin
                        state      <= RD_REQ;
                        wr_fin     <= 1'b0;
                        rd_fin     <= 1'b0;
                        addr       <= rd_addr;
                    end else begin
                        state      <= IDLE;
                    end
                end
                WR_REQ: begin
                    mem[addr + 0]  <= wr_data[31:24];
                    mem[addr + 1]  <= wr_data[23:16];
                    mem[addr + 2]  <= wr_data[15: 8];
                    mem[addr + 3]  <= wr_data[ 7: 0];
                    state          <= WR_FIN;
                end
                RD_REQ: begin
                    rd_data[31:24] <= mem[addr + 0];
                    rd_data[23:16] <= mem[addr + 1];
                    rd_data[15: 8] <= mem[addr + 2];
                    rd_data[ 7: 0] <= mem[addr + 3];
                    state          <= RD_FIN;
                end
                WR_FIN: begin
                    state  <= IDLE;
                    wr_fin <= 1'b1;
                end
                RD_FIN: begin
                    state  <= IDLE;
                    rd_fin <= 1'b1;
                end
            endcase
        end
    end
endmodule

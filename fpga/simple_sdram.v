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
    localparam IDLE    = 4'd0;
    localparam WR_REQ  = 4'd1;
    localparam WR_FIN  = 4'd2;
    localparam RD_REQ1 = 4'd3;
    localparam RD_REQ2 = 4'd4;
    localparam RD_REQ3 = 4'd5;
    localparam RD_FIN  = 4'd6;

    reg [7:0] mem[MEM_SIZE-1:0];
    reg [3:0] state = IDLE;
    reg [31:0] addr;

    always @(posedge clk or posedge reset) begin
        if (reset) begin
            state   <= IDLE;
            rd_fin  <= 1'b0;
            wr_fin  <= 1'b0;
        end else begin
            case (state)
                IDLE: begin
                    if (wr_req & ~wr_fin) begin
                        state      <= WR_REQ;
                        addr       <= wr_addr;
                    end else if (rd_req & ~rd_fin) begin
                        state      <= RD_REQ1;
                        addr       <= rd_addr;
                    end else begin
                        state      <= IDLE;
                        wr_fin     <= wr_req & wr_fin;
                        rd_fin     <= rd_req & rd_fin;
                    end
                end
                WR_REQ: begin
                    mem[addr + 0]  <= wr_data[31:24];
                    mem[addr + 1]  <= wr_data[23:16];
                    mem[addr + 2]  <= wr_data[15: 8];
                    mem[addr + 3]  <= wr_data[ 7: 0];
                    state          <= WR_FIN;
                end
                RD_REQ1: begin
                    state          <= #1 RD_REQ2;
                end
                RD_REQ2: begin
                    state          <= #1 RD_REQ3;
                end
                RD_REQ3: begin
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

    always @(negedge clk or posedge reset) begin
        if (reset) begin
            rd_data <= 32'b0;
        end else if (state == RD_REQ3) begin
            rd_data[31:24] <= mem[addr + 0];
            rd_data[23:16] <= mem[addr + 1];
            rd_data[15: 8] <= mem[addr + 2];
            rd_data[ 7: 0] <= mem[addr + 3];
        end
    end
endmodule

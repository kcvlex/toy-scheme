module SIMPLE_SDRAM #(
    parameter MEM_SIZE = 65536,
    parameter AWIDTH = 25
) (
    input wire CLK, RST_X,
    input wire wr_req,
    input wire rd_req,
    input wire [31:0] wr_data,
    input wire [AWIDTH-1:0] addr,
    output wire wr_ack,
    output wire rd_ack,
    output wire [31:0] rd_data
);
    localparam IDLE   = 3'd0;
    localparam WR_REQ = 3'd1;
    localparam WR_FIN = 3'd2;
    localparam RD_REQ = 3'd3;
    localparam RD_FIN = 3'd4;

    reg [7:0] mem[MEM_SIZE-1:0];
    reg [2:0] state = IDLE, next_state = IDLE;
    reg [31:0] r_wd_data, r_rd_data;
    reg iclk = 1'b0;

    initial forever #80 iclk = ~iclk;

    always @(state or wr_req or rd_req) begin
        case (state)
            IDLE: begin
                if (wr_req)      next_state <= WR_REQ;
                else if (rd_req) next_state <= RD_REQ;
                else             next_state <= IDLE;
            end
            WR_REQ:              next_state <= WR_FIN;
            WR_FIN:              next_state <= IDLE;
            RD_REQ:              next_state <= RD_FIN;
            RD_FIN:              next_state <= IDLE;
        endcase
    end

    always @(posedge CLK) state <= next_state;

    always @(negedge iclk) begin
        if (wr_req) begin
            mem[addr + 0] <= wr_data[31:24];
            mem[addr + 1] <= wr_data[23:16];
            mem[addr + 2] <= wr_data[15:8];
            mem[addr + 3] <= wr_data[7:0];
        end
        if (rd_req) begin
            r_rd_data[31:24] <= mem[addr + 0];
            r_rd_data[23:16] <= mem[addr + 1];
            r_rd_data[15:8]  <= mem[addr + 2];
            r_rd_data[7:0]   <= mem[addr + 3];
        end
    end

    assign wr_ack = (state == WR_FIN);
    assign rd_ack = (state == RD_FIN);
    assign rd_data = r_rd_data;
endmodule

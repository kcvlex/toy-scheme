module MEM_STAGE #(
    parameter AWIDTH = 25
) (
    input wire CLK,
    input wire RST_X,

    ///// REQ and ACK /////
    input  wire mem_req,
    output wire mem_ack,

    ///// INPUT /////
    input wire              wr_mem,
    input wire              rd_mem,
    input wire [AWIDTH-1:0] mem_addr,
    input wire [31:0]       mem_wr_data,

    ///// OUTPUT /////
    output wire [31:0] mem_rd_data,

    ///// SDRAM /////
    input  wire [31:0]       sdram_rd_data,
    input  wire              sdram_rd_ack,
    input  wire              sdram_wr_ack,
    output wire              sdram_rd_req,
    output wire              sdram_wr_req,
    output wire [AWIDTH-1:0] sdram_addr,
    output wire [31:0]       sdram_wr_data
);
    localparam IDLE       = 3'd0;
    localparam MEM_WR_REQ = 3'd1;
    localparam MEM_RD_REQ = 3'd2;
    localparam MEM_WR_FIN = 3'd3;
    localparam MEM_RD_FIN = 3'd4;

    reg [2:0] state = IDLE, next_state = IDLE;
    reg [31:0] r_sdram_rd_data = 32'b0;
    reg r_sdram_wr_req = 1'b0, r_sdram_rd_req = 1'b0;

    always @(state or mem_req or sdram_wr_ack or sdram_rd_ack) begin
        case (state)
            IDLE: begin
                if (!mem_req)    next_state <= IDLE;
                else if (wr_mem) next_state <= MEM_WR_REQ;
                else if (rd_mem) next_state <= MEM_RD_REQ;
                else             next_state <= MEM_WR_FIN;  // FIXME
            end
            MEM_WR_REQ: begin
                if (sdram_wr_ack) next_state <= MEM_WR_FIN;
                else              next_state <= MEM_WR_REQ;
            end
            MEM_RD_REQ: begin
                if (sdram_rd_ack) next_state <= MEM_RD_FIN;
                else              next_state <= MEM_RD_REQ;
            end
            MEM_WR_FIN:           next_state <= IDLE;
            MEM_RD_FIN:           next_state <= IDLE;
        endcase
    end

    always @(posedge CLK) state <= next_state;
    always @(state) begin
        r_sdram_wr_req <= (state == MEM_WR_REQ);
        r_sdram_rd_req <= (state == MEM_RD_REQ);
        r_sdram_rd_data <= (state == MEM_RD_FIN ? sdram_rd_data : r_sdram_rd_data);
    end

    assign #1 mem_ack       = (state == MEM_RD_FIN || state == MEM_WR_FIN);
    assign #1 sdram_addr    = mem_addr;
    assign #1 sdram_wr_data = mem_wr_data;
    assign #1 mem_rd_data   = r_sdram_rd_data;
    assign #1 sdram_wr_req  = r_sdram_wr_req;
    assign #1 sdram_rd_req  = r_sdram_rd_req;
endmodule

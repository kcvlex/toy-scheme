module MEM_STAGE(
    input wire clk,
    input wire reset,

    ///// REQ and ACK /////
    input  wire mem_wr_req,
    input  wire mem_rd_req,
    output reg  mem_wr_fin,
    output reg  mem_rd_fin,

    ///// MEM /////
    input wire [31:0] mem_wr_addr,
    input wire [31:0] mem_wr_data,
    input wire [31:0] mem_rd_addr,
    output reg [31:0] mem_rd_data,

    ///// SDRAM /////
    input  wire [31:0] sdram_rd_data,
    output wire [31:0] sdram_rd_addr,
    output reg         sdram_rd_req,
    input  wire        sdram_rd_fin,

    output wire [31:0] sdram_wr_data,
    output wire [31:0] sdram_wr_addr,
    output reg         sdram_wr_req,
    input  wire        sdram_wr_fin
);
    localparam IDLE   = 3'd0;
    localparam WR_REQ = 3'd1;
    localparam RD_REQ = 3'd2;
    localparam FIN    = 3'd3;

    reg [2:0] state = IDLE;

    always @(posedge clk or posedge reset) begin
        if (reset) begin
            state        <= #1 IDLE;
            mem_wr_fin   <= #1 1'b0;
            mem_rd_fin   <= #1 1'b0;
            mem_rd_data  <= #1 32'b0;
            sdram_wr_req <= #1 1'b0;
            sdram_rd_req <= #1 1'b0;
        end else begin
            case (state)
                IDLE: begin
                    mem_wr_fin        <= #1 1'b0;
                    mem_rd_fin        <= #1 1'b0;
                    if (mem_wr_req) begin
                        state         <= #1 WR_REQ;
                        sdram_wr_req  <= #1 1'b1;
                    end else if (mem_rd_req) begin
                        state         <= #1 RD_REQ;
                        sdram_rd_req  <= #1 1'b1;
                    end else begin
                        state         <= #1 IDLE;
                    end
                end
                WR_REQ: begin
                    if (sdram_wr_fin) begin
                        state         <= #1 FIN;
                        mem_wr_fin    <= #1 1'b1;
                        sdram_wr_req  <= #1 1'b0;
                    end else begin
                        state         <= #1 WR_REQ;
                    end
                end
                RD_REQ: begin
                    if (sdram_rd_fin) begin
                        state         <= #1 FIN;
                        mem_rd_fin    <= #1 1'b1;
                        sdram_rd_req  <= #1 1'b0;
                        mem_rd_data   <= #1 sdram_rd_data;
                    end else begin
                        state         <= #1 RD_REQ;
                    end
                end
                FIN: begin
                    state             <= #1 IDLE;
                end
                default: begin
                    state             <= #1 state;
                end
            endcase
        end
    end

    assign #1 sdram_wr_addr = mem_wr_addr;
    assign #1 sdram_wr_data = mem_wr_data;
    assign #1 sdram_rd_addr = mem_rd_addr;
endmodule

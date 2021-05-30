module INSTR_FETCH(
    input wire        clk, 
    input wire        reset,
    input wire [31:0] pc,
    
    input  wire        req,
    output reg         fin,
    output reg  [31:0] instr,

    input  wire        sdram_rd_fin,
    output reg         sdram_rd_req,
    input  wire [31:0] sdram_rd_data,
    output wire [31:0] sdram_rd_addr
);
    localparam IDLE = 2'd0;
    localparam REQ  = 2'd1;
    localparam FIN  = 2'd2;

    reg [1:0] state = IDLE;

    always @(negedge clk) begin
        if (reset) begin
            state        <= #1 IDLE;
            sdram_rd_req <= #1 1'b0;
        end else begin
            case (state)
                IDLE: begin
                    if (req) begin
                        state        <= #1 REQ;
                        sdram_rd_req <= #1 1'b1;
                    end else begin
                        state        <= #1 IDLE;
                        sdram_rd_req <= #1 1'b0;
                    end
                end
                REQ: begin
                    if (sdram_rd_fin) begin
                        state        <= #1 FIN;
                        sdram_rd_req <= #1 1'b0;
                        instr        <= #1 sdram_rd_data;
                        fin          <= #1 1'b1;
                    end else begin
                        state        <= #1 REQ;
                    end
                end
                FIN: begin
                    state            <= #1 IDLE;
                    fin              <= #1 1'b0;
                end
                default: begin
                    state            <= #1 state;
                end
            endcase
        end
    end

    assign #1 sdram_rd_addr = pc;
endmodule

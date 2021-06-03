module INSTR_FETCH(
    input wire        clk, 
    input wire        reset,
    input wire [31:0] pc,
   
    input  wire        req,
    output reg         fin,
    output reg  [31:0] instr,

    input  wire        dram_rd_fin,
    output reg         dram_rd_req,
    input  wire [31:0] dram_rd_data,
    output reg  [31:0] dram_rd_addr
);
    localparam IDLE = 2'd0;
    localparam REQ  = 2'd1;
    localparam FIN  = 2'd2;

    reg [1:0] state = IDLE;

    always @(posedge clk or posedge reset) begin
        if (reset) begin
            state        <= #1 IDLE;
            dram_rd_req  <= #1 1'b0;
            dram_rd_addr <= 32'b0;
        end else begin
            case (state)
                IDLE: begin
                    if (req) begin
                        state        <= #1 REQ;
                        dram_rd_req  <= #1 1'b1;
                    end else begin
                        state        <= #1 IDLE;
                        dram_rd_req  <= #1 1'b0;
                    end
                    dram_rd_addr <= pc;
                end
                REQ: begin
                    if (dram_rd_fin) begin
                        state        <= #1 FIN;
                        dram_rd_req <= #1 1'b0;
                        instr        <= #1 dram_rd_data;
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

    // assign #1 dram_rd_addr = pc;
endmodule

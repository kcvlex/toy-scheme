module INSTR_FETCH #(
    parameter AWIDTH = 25
) (
    input wire CLK, RST_X,
    input wire [31:0] pc,
    
    input wire fetch_req,
    output wire fetch_ack,
    output wire [31:0] instr,

    input wire sdram_rd_ack,
    output wire sdram_rd_req,
    input wire [31:0] sdram_rd_data,
    output wire [AWIDTH-1:0] sdram_addr
);
    localparam IDLE = 2'd0;
    localparam REQ  = 2'd1;
    localparam FIN  = 2'd2;

    reg [1:0] state = IDLE, next_state = IDLE;
    reg [31:0] r_instr = 32'b0;

    always @(state or sdram_rd_ack or fetch_req) begin
        case (state) 
            IDLE: begin
                if (fetch_req)    next_state <= REQ;
                else              next_state <= IDLE;
            end
            REQ: begin
                if (sdram_rd_ack) next_state <= FIN;
                else              next_state <= REQ;
            end
            FIN:                  next_state <= IDLE;
        endcase
    end

    always @(posedge CLK) state <= next_state;

    always @(state) if (state == FIN) r_instr <= sdram_rd_data;

    assign #1 sdram_rd_req = (state == REQ);
    assign #1 sdram_addr = pc[AWIDTH-1:0];
    assign #1 fetch_ack = (state == FIN);
    assign #1 instr = r_instr;
endmodule

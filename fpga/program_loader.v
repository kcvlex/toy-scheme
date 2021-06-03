module PROGRAM_LOADER(
	
    //////////// CLOCK //////////
    input wire clk,
    input wire reset,
    
    //////////// UART //////////
    input  wire       rx_ack,
    input  wire [7:0] rx_data,
    input  wire       tx_ready,
    output reg        tx_req,
    output reg  [7:0] tx_data,
	
    //////////// SDRAM //////////
    input  wire        wr_fin,
    output wire [31:0] wr_addr,
    output wire [31:0] wr_data,
    output reg         wr_req,
    
    //////////// ENTRY POINT //////////
    output reg [31:0] entry_addr,

    //////////// REQ and RES //////////
    input  wire req
);
    localparam IDLE     = 4'd0;
    localparam RX_REQ   = 4'd1;
    localparam RX_FIN   = 4'd2;
    localparam DRAM_REQ = 4'd3;
    localparam DRAM_FIN = 4'd4;
    localparam TX_REQ   = 4'd5;
    localparam TX_FIN   = 4'd6;

    // Received Data
    reg [63:0] rx_buffer, tx_buffer;

    reg [3:0] rx_rem, tx_rem;
    reg [3:0] state;

    always @(negedge clk or posedge reset) begin
        if (reset) begin
            rx_buffer  <= 64'b0;
            tx_buffer  <= 64'b0;
            wr_req     <= 1'b0;
            tx_req     <= 1'b0;
            state      <= IDLE;
            entry_addr <= 32'b0;
            rx_rem     <= 4'b0;
            tx_rem     <= 4'b0;
        end else if (req) begin
            case (state)
                IDLE: begin
                    if (rx_ack) begin
                        state     <= RX_REQ;
                        rx_rem    <= 4'd7;
                        rx_buffer <= (rx_buffer << 8) | { 56'b0, rx_data };
                    end else begin
                        state     <= IDLE;
                    end
                end
                RX_REQ: begin
                    if (rx_ack) begin
                        rx_buffer <= (rx_buffer << 8) | { 56'b0, rx_data };
                        if (rx_rem - 4'b1 == 4'b0) begin
                            state <= RX_FIN;
                        end else begin
                            state <= RX_REQ;
                        end
                        rx_rem    <= rx_rem - 4'b1;
                    end else begin
                        state     <= RX_REQ;
                    end
                end
                RX_FIN: begin
                    state         <= DRAM_REQ;
                    wr_req        <= 1'b1;
                end
                DRAM_REQ: begin
                    if (wr_fin) begin
                        state     <= DRAM_FIN;
                        wr_req    <= 1'b0;
                    end else begin
                        state     <= DRAM_REQ;
                    end
                end
                DRAM_FIN: begin
                    state         <= TX_REQ;
                    tx_rem        <= 4'd8;
                    tx_buffer     <= rx_buffer;
                end
                TX_REQ: begin
                    if (tx_rem == 4'b0) begin
                        state     <= TX_FIN;
                        tx_req    <= 1'b0;
                    end else if (tx_ready) begin
                        state     <= TX_REQ;
                        tx_req    <= 1'b1;
                        tx_data   <= tx_buffer[7:0];
                        tx_rem    <= tx_rem - 4'b1;
                        tx_buffer <= tx_buffer >> 8;
                    end else begin
                        state     <= TX_REQ;
                        tx_req    <= 1'b0;
                    end
                end
                TX_FIN: begin
                    state         <= IDLE;
                end
                default: begin
                    state         <= state;
                end
            endcase
        end
    end

    assign wr_addr = rx_buffer[63:32];
    assign wr_data = rx_buffer[31:0];
endmodule

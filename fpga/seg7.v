module SEG7(
    input  wire       clk,
    input  wire       reset,
    input  wire [3:0] din,
    input  wire       none,
    input  wire       num,
    input  wire       dot,
    input  wire       hi_1,
    input  wire       hi_2,
    input  wire       lo_1,
    input  wire       lo_2,
    output wire [7:0] dout
);
    /*
     *  ----0----
     *  |       |
     *  5       1
     *  |       |
     *  ----6----
     *  |       |
     *  4       2
     *  |       |
     *  ----3----
     *
     */

    reg [6:0] r_dout;

    always @(posedge clk or posedge reset) begin
        if (reset) begin
            r_dout <= 7'b0;
        end else begin
            if (none) begin
                r_dout <= 7'b1_000000;
            end else if (hi_1) begin
                r_dout <= 7'b1_11_0_11_0;
            end else if (hi_2) begin
                r_dout <= 7'b00_1_0000;
            end else if (lo_1) begin
                r_dout <= 7'b0_111_000;
            end else if (lo_2) begin
                r_dout <= 7'b01_0_111_00;
            end else if (num) begin
                case (din)
                    4'h0: r_dout <= 7'b0111111;
                    4'h1: r_dout <= 7'b0000110;
                    4'h2: r_dout <= 7'b1011011;
                    4'h3: r_dout <= 7'b1001111;
                    4'h4: r_dout <= 7'b1100110;
                    4'h5: r_dout <= 7'b1101101;
                    4'h6: r_dout <= 7'b1111101;
                    4'h7: r_dout <= 7'b0000111;
                    4'h8: r_dout <= 7'b1111111;
                    4'h9: r_dout <= 7'b1100111;
                    4'ha: r_dout <= 7'b1110111;
                    4'hb: r_dout <= 7'b1111100;
                    4'hc: r_dout <= 7'b0111001;
                    4'hd: r_dout <= 7'b1011110;
                    4'he: r_dout <= 7'b1111001;
                    4'hf: r_dout <= 7'b1110001;
                endcase
            end else begin
                r_dout <= 7'b1_000000;
            end
        end
    end

    assign dout = { ~dot, ~r_dout };
endmodule

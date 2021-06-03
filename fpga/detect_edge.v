module DETECT_EDGE #(
    parameter POSEDGE = 1
) (
    input wire CLK,
    input wire RESET,
    input wire SIGNAL,
    output wire EDGE
);
    reg state, prev_state;

    always @(posedge CLK or posedge RESET) begin
        if (RESET) begin
            state      <= #1 (POSEDGE ? 1'b0 : 1'b1);
            prev_state <= #1 (POSEDGE ? 1'b0 : 1'b1);
        end else begin
            state      <= #1 SIGNAL;
            prev_state <= #1 state;
        end
    end
    
    assign #1 EDGE = POSEDGE ? (state & ~prev_state)
                   :           (~state & prev_state);
endmodule

module DETECT_POSEDGE(
    input wire CLK,
    input wire RESET,
    input wire SIGNAL,
    output wire EDGE
);
    DETECT_EDGE #(
        .POSEDGE(1)
    ) detect(
        .CLK(CLK),
        .RESET(RESET),
        .SIGNAL(SIGNAL),
        .EDGE(EDGE)
    );
endmodule

module DETECT_NEGEDGE(
    input wire CLK,
    input wire RESET,
    input wire SIGNAL,
    output wire EDGE
);
    DETECT_EDGE #(
        .POSEDGE(0)
    ) detect(
        .CLK(CLK),
        .RESET(RESET),
        .SIGNAL(SIGNAL),
        .EDGE(EDGE)
    );
endmodule

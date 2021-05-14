module DETECT_EDGE #(
    parameter POSEDGE = 1
) (
    input wire CLK,
    input wire SIGNAL,
    output wire EDGE
);
    reg prev_state = (POSEDGE ? 1'b0 : 1'b1);
    reg state      = (POSEDGE ? 1'b0 : 1'b1);

    always @(posedge CLK) begin
        prev_state = #1 state;
        state      = #1 SIGNAL;
    end
    
    assign #1 EDGE = (POSEDGE ? (state & ~prev_state)
                              : (~state & prev_state));
endmodule

module DETECT_POSEDGE(
    input wire CLK,
    input wire SIGNAL,
    output wire EDGE
);
    DETECT_EDGE #(
        .POSEDGE(1)
    ) detect(
        .CLK(CLK),
        .SIGNAL(SIGNAL),
        .EDGE(EDGE)
    );
endmodule

module DETECT_NEGEDGE(
    input wire CLK,
    input wire SIGNAL,
    output wire EDGE
);
    DETECT_EDGE #(
        .POSEDGE(0)
    ) detect(
        .CLK(CLK),
        .SIGNAL(SIGNAL),
        .EDGE(EDGE)
    );
endmodule

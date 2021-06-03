module ALU(
    input  wire        clk,
    input  wire        reset,
    input  wire        req,
    input  wire [4:0]  op,
    input  wire [31:0] lhs,
    input  wire [31:0] rhs,
    output reg  [31:0] res
);
    `include "alu.hv"

    wire signed [31:0] s_lhs = $signed(lhs);
    wire signed [31:0] s_rhs = $signed(rhs);

    always @(posedge clk or posedge reset) begin
        if (reset) begin
            res <= #1 32'b0;
        end else if (req) begin
            case (op)
                ADD:     res <= #1 lhs + rhs;
                SUB:     res <= #1 lhs - rhs;
                SLL:     res <= #1 lhs << rhs;
                SLT:     res <= #1 { 31'b0, s_lhs < s_rhs };
                SLTU:    res <= #1 { 31'b0, lhs < rhs };
                XOR:     res <= #1 lhs ^ rhs;
                SRL:     res <= #1 lhs >> rhs;
                SRA:     res <= #1 lhs >>> rhs;
                OR:      res <= #1 lhs | rhs;
                AND:     res <= #1 lhs & rhs;
                EQ:      res <= #1 { 31'b0, lhs == rhs };
                NEQ:     res <= #1 { 31'b0, lhs != rhs };
                LT:      res <= #1 { 31'b0, s_lhs < s_rhs };
                GE:      res <= #1 { 31'b0, s_lhs >= s_rhs };
                LTU:     res <= #1 { 31'b0, lhs < rhs };
                GEU:     res <= #1 { 31'b0, lhs >= rhs };
                default: res <= #1 32'b0;
            endcase
        end
    end
endmodule

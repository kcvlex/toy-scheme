module ALU(
    input  wire        clk,
    input  wire        reset,
    input  wire [4:0]  op,
    input  wire [31:0] lhs,
    input  wire [31:0] rhs,
    output reg  [31:0] res
);
    `include "alu.hv"

    wire signed [31:0] s_lhs = $signed(lhs);
    wire signed [31:0] s_rhs = $signed(rhs);

    always @(reset or op or lhs or rhs) begin
        case (op)
            ADD:     res <= #1 lhs + rhs;
            SUB:     res <= #1 lhs - rhs;
            SLL:     res <= #1 lhs << rhs;
            SLT:     res <= #1 s_lhs < s_rhs;
            SLTU:    res <= #1 lhs < rhs;
            XOR:     res <= #1 lhs ^ rhs;
            SRL:     res <= #1 lhs >> rhs;
            SRA:     res <= #1 lhs >>> rhs;
            OR:      res <= #1 lhs | rhs;
            AND:     res <= #1 lhs & rhs;
            EQ:      res <= #1 lhs == rhs;
            NEQ:     res <= #1 lhs != rhs;
            LT:      res <= #1 s_lhs < s_rhs;
            GE:      res <= #1 s_lhs >= s_rhs;
            LTU:     res <= #1 lhs < rhs;
            GEU:     res <= #1 lhs >= rhs;
            default: res <= #1 0;
        endcase
    end
endmodule

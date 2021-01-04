module ALU(
    input wire CLK, RST_X,
    input wire [4:0] op,
    input wire [31:0] lhs, rhs,
    output wire [31:0] res
);
    `include "alu.hv"

    wire signed [31:0] s_lhs = $signed(lhs);
    wire signed [31:0] s_rhs = $signed(rhs);
    reg [31:0] r_res;

    always @(RST_X, op, lhs, rhs) begin
        if (!RST_X) begin
            r_res <= #1 0;
        end else begin
            case (op)
                ADD:  r_res <= #1 lhs + rhs;
                SUB:  r_res <= #1 lhs - rhs;
                SLL:  r_res <= #1 lhs << rhs;
                SLT:  r_res <= #1 s_lhs < s_rhs;
                SLTU: r_res <= #1 lhs < rhs;
                XOR:  r_res <= #1 lhs ^ rhs;
                SRL:  r_res <= #1 lhs >> rhs;
                SRA:  r_res <= #1 lhs >>> rhs;
                OR:   r_res <= #1 lhs | rhs;
                AND:  r_res <= #1 lhs & rhs;
                EQ:   r_res <= #1 lhs == rhs;
                default: r_res <= #1 0;
            endcase
        end
    end

    assign #1 res = r_res;
endmodule


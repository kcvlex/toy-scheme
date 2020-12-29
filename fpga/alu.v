module ALU(
    input wire CLK, RST_X,
    input wire [4:0] op,
    input wire [31:0] lhs, rhs,
    output wire [31:0] res
);
    `include "alu.hv"

    wire [31:0] s_lhs = $signed(lhs);
    wire [31:0] s_rhs = $signed(rhs);
    reg [31:0] r_res;

    always @(*) begin
        if (!RST_X) begin
            r_res <= #5 0;
        end else begin
            case (op)
                ADD:  r_res <= #5 lhs + rhs;
                SUB:  r_res <= #5 lhs - rhs;
                SLL:  r_res <= #5 lhs << rhs;
                SLT:  r_res <= #5 s_lhs < s_rhs;
                SLTU: r_res <= #5 lhs < rhs;
                XOR:  r_res <= #5 lhs ^ rhs;
                SRL:  r_res <= #5 lhs >> rhs;
                SRA:  r_res <= #5 lhs >>> rhs;
                OR:   r_res <= #5 lhs | rhs;
                AND:  r_res <= #5 lhs & rhs;
                EQ:   r_res <= #5 lhs == rhs;
                default: r_res <= #5 0;
            endcase
        end
    end

    assign #5 res = r_res;
endmodule


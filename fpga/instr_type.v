module INSTR_TYPE(
    input wire [4:0] opcode,
    output wire [2:0] instr_type
);
    `include "instr_type.hv"
    wire [1:0] base_opcode_map_row = opcode[4:3];
    wire [2:0] base_opcode_map_col = opcode[2:0];
    
    assign instr_type = f(base_opcode_map_row, base_opcode_map_col);
    
    function [2:0] f(input [1:0] in0, input[2:0] in1);
        case (in1)
            3'b000: f = (in0 == 2'b00 ? I_TYPE :    // LOAD
                         in0 == 2'b01 ? S_TYPE :    // STORE
                         in0 == 2'b10 ? R4_TYPE :   // FIXME : what's MADD ??
                                        B_TYPE);    // BRANCH
            3'b001: f = (in0 == 2'b11 ? I_TYPE :    // JALR
                                        I_TYPE);    // FIXME
            3'b010: f = I_TYPE;                     // FIXME
            3'b011: f = (in0 == 2'b11 ? J_TYPE :    // JAL
                                        J_TYPE);    // FIXME
            3'b100: f = (in0 == 2'b00 ? I_TYPE :    // OP-IMM
                         in0 == 2'b01 ? R_TYPE :    // OP
                                        R_TYPE);    // FIXME
            3'b101: f = (in0 == 2'b01 ? U_TYPE :    // LUI
                                        U_TYPE);    // FIXME
            3'b110: f = (in0 == 2'b00 ? I_TYPE :    // OP-IMM-32
                         in0 == 2'b01 ? R_TYPE :    // OP-32
                                        R_TYPE);    // FIXME
            default: f = 3'b0;
        endcase
    endfunction
endmodule

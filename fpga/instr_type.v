module INSTR_TYPE(
    input wire [4:0] opcode,
    output wire [2:0] instr_format,
    output wire [4:0] instr_type
);
    `include "instr_type.hv"
    wire [1:0] #5 base_opcode_map_row = opcode[4:3];
    wire [2:0] #5 base_opcode_map_col = opcode[2:0];
    
    assign #5 { instr_format, instr_type } = f(base_opcode_map_row, base_opcode_map_col);
    
    function [7:0] f(input [1:0] in0, input[2:0] in1);
        case (in1)
            3'b000: f = #5 (in0 == 2'b00 ? { I_TYPE, LOAD } :
                            in0 == 2'b01 ? { S_TYPE, STORE } :
                            in0 == 2'b10 ? { R4_TYPE, MADD } :     // FIXME : what's MADD ??
                                           { B_TYPE, BRANCH });

            3'b001: f = #5 (in0 == 2'b11 ? { I_TYPE, JALR } :
                                           { I_TYPE, JALR });      // FIXME
                            
            3'b010: f = #5 (in0 == 2'b10 ? { I_TYPE, NMSUB } :
                                           { I_TYPE, NMSUB });     // FIXME
            3'b011: f = #5 (in0 == 2'b11 ? { J_TYPE, JAL } :
                                           { J_TYPE, JAL});        // FIXME
            3'b100: f = #5 (in0 == 2'b00 ? { I_TYPE, OP_IMM } :
                            in0 == 2'b01 ? { R_TYPE, OP } :
                                           { R_TYPE, OP });        // FIXME
            3'b101: f = #5 (in0 == 2'b01 ? { U_TYPE, LUI } :
                                           { U_TYPE, LUI });       // FIXME
            3'b110: f = #5 (in0 == 2'b00 ? { I_TYPE, OP_IMM_32 } :
                            in0 == 2'b01 ? { R_TYPE, OP_32 } :
                                           { R_TYPE, OP_32 });     // FIXME
            default: f = #5 3'b0;
        endcase
    endfunction
endmodule

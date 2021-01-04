module INSTR_TYPE(
    input wire CLK,
    input wire [4:0] opcode,
    output wire [2:0] instr_format,
    output wire [4:0] instr_type
);
    `include "instr_type.hv"
    
    wire [1:0] #1 op43 = opcode[4:3];
    wire [2:0] #1 op20 = opcode[2:0];
    reg [7:0] f;

    always @(*) begin
        case (op20)
            3'b000: f = #1 (op43 == 2'b00 ? { I_TYPE, LOAD } :
                            op43 == 2'b01 ? { S_TYPE, STORE } :
                            op43 == 2'b10 ? { R4_TYPE, MADD } :     // FIXME : what's MADD ??
                                            { B_TYPE, BRANCH });

            3'b001: f = #1 (op43 == 2'b11 ? { I_TYPE, JALR } :
                                            { I_TYPE, JALR });      // FIXME
                            
            3'b010: f = #1 (op43 == 2'b10 ? { I_TYPE, NMSUB } :
                                            { I_TYPE, NMSUB });     // FIXME
            3'b011: f = #1 (op43 == 2'b11 ? { J_TYPE, JAL } :
                                            { J_TYPE, JAL});        // FIXME
            3'b100: f = #1 (op43 == 2'b00 ? { I_TYPE, OP_IMM } :
                            op43 == 2'b01 ? { R_TYPE, OP } :
                                            { R_TYPE, OP });        // FIXME
            3'b101: f = #1 (op43 == 2'b01 ? { U_TYPE, LUI } :
                                            { U_TYPE, LUI });       // FIXME
            3'b110: f = #1 (op43 == 2'b00 ? { I_TYPE, OP_IMM_32 } :
                            op43 == 2'b01 ? { R_TYPE, OP_32 } :
                                            { R_TYPE, OP_32 });     // FIXME
            default: f = #1 3'b0;
        endcase
    end

    assign #1 { instr_format, instr_type } = f;
endmodule

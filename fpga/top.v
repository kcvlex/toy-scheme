`default_nettype none

module top();
    reg CLK, RST_X;
    initial begin CLK = 1; forever #50 CLK = ~CLK; end
    initial begin RST_X = 0; #125 RST_X = 1;       end
    initial begin #1000 $finish();                  end
    initial begin $dumpfile("wave.vcd"); $dumpvars(0, p); end

    initial begin
        /*
        p.imem.mem[0] = 32'd0;
        p.imem.mem[1] = { 12'd10, 5'd0, 3'b0, 5'd5, 7'b0010011 };
        p.imem.mem[2] = { 12'd32, 5'd0, 3'b0, 5'd6, 7'b0010011 };
        p.imem.mem[3] = { 7'd0, 5'd6, 5'd5, 3'b0, 5'd7, 7'b0110011 };
        */
        p.imem.mem[0] = 32'd0;
        p.imem.mem[1] = 32'b11111101000000000000001010010011;
        p.imem.mem[2] = 32'b00000010101000000000001100010011;
        p.imem.mem[3] = 32'b00000000011000101000001110110011;
    end
    
    always @(CLK) begin
        $write("%d: %d %d %d %d, %d %d %d, %d %d %d, %d %d, %d\n", 
                $stime, 
                RST_X, p.cpu.Id_valid, p.cpu.Ex_valid, p.cpu.Wb_valid,
                p.cpu.rs1, p.cpu.rs2, p.cpu.rd,
                $signed(p.cpu.regfile.x[5]), $signed(p.cpu.regfile.x[6]), $signed(p.cpu.regfile.x[7]),
                $signed(p.cpu.rrs1), $signed(p.cpu.rrs2),
                $signed(p.cpu.Ex_wd_reg));
    end

    PROCESSOR p(CLK, RST_X);
endmodule


module PROCESSOR(
    input wire CLK,
    input wire RST_X
);
    reg [31:0] PC;
    wire [31:0] instr, next_pc;

    always @(posedge RST_X) PC <= #5 0;
    always @(posedge CLK) if (RST_X) PC <= next_pc;

    IMEM imem(CLK, RST_X, PC, instr);
    CPU cpu(CLK, RST_X, PC, instr, next_pc);
endmodule

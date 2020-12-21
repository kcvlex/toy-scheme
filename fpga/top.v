`default_nettype none

module top();
    reg CLK, RST_X;
    initial begin CLK = 1; forever #50 CLK = ~CLK; end
    initial begin RST_X = 0; #125 RST_X = 1;       end
    initial begin #900 $finish();                  end
    initial begin $dumpfile("wave.vcd"); $dumpvars(0, p); end

    initial begin
        p.imem.mem[0] = 32'd0;
        p.imem.mem[1] = { 12'd10, 5'd0, 3'b0, 5'd5, 7'b0010011 };
        p.imem.mem[2] = { 12'd32, 5'd0, 3'b0, 5'd6, 7'b0010011 };
        p.imem.mem[3] = { 7'd0, 5'd6, 5'd5, 3'b0, 5'd7, 7'b0110011 }; 
    end
    
    always @(CLK) begin
        $write("%d: %d %d %d %d %d %d %d %d %d %d %d\n", 
                $stime, RST_X, p.cpu.we,
                p.cpu.rs1, p.cpu.rs2, p.cpu.rd,
                p.cpu.regfile.x[5], p.cpu.regfile.x[6], p.cpu.regfile.x[7],
                p.cpu.rrs1, p.cpu.rrs2,
                p.cpu.result);
    end

    PROCESSOR p(CLK, RST_X);
endmodule


module PROCESSOR(
    input wire CLK,
    input wire RST_X
);
    reg [31:0] PC;
    wire [31:0] instr;

    always @(posedge RST_X) PC <= #5 0;
    always @(posedge CLK) if (RST_X) PC <= #5 PC + 4;

    IMEM imem(CLK, RST_X, PC, instr);
    CPU cpu(CLK, RST_X, PC, instr);
endmodule

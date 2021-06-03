module SDRAM_INITIALIZER(

    input  wire         clk,
    input  wire         reset,
    output reg          fin,
    
    output wire [12:0]  DRAM_ADDR,
    output wire [1:0]   DRAM_BA,
    output wire         DRAM_CAS_N,
    output wire         DRAM_CS_N,
    output wire         DRAM_LDQM,
    output wire         DRAM_UDQM,
    output wire         DRAM_RAS_N,
    output wire         DRAM_WE_N
);
    // 100us
    // if clock is 100MHz, wait 100'000'000/1000'0 = 10'000

    localparam WAIT_INIT_CYCLE = 32'd20_000;
    localparam AUTO_REFRESH_TIMES = 8;

    `include "sdram_params.hv"

    localparam STATE_POWER_ON  = 4'd1;
    localparam STATE_PRECHARGE = 4'd2;
    localparam STATE_REFRESH   = 4'd3;
    localparam STATE_MODE_REG  = 4'd4;
    localparam STATE_INIT_FIN  = 4'd5;

    localparam MODE_REG = 10'b0_00_010_0_001;

    reg [3:0]  state = STATE_POWER_ON;
    reg [3:0]  command = CMD_NOP;
    reg [31:0] wait_counter = 32'b0;
    reg [31:0] ref_counter = 32'b0;
    
    // Internal
    reg [12:0] inter_addr;

    always @(posedge clk or posedge reset) begin
        if (reset) begin
            state        <= STATE_POWER_ON;
            wait_counter <= 32'b0;
            ref_counter  <= 32'b0;
            fin          <= 1'b0;
        end else begin
            case (state)
                STATE_POWER_ON: begin
                    if (WAIT_INIT_CYCLE <= wait_counter) begin
                        state        <= STATE_PRECHARGE;
                        command      <= CMD_PRECHARGE;
                        wait_counter <= 32'b0;
                    end else begin
                        state        <= STATE_POWER_ON;
                        command      <= CMD_NOP;
                        wait_counter <= wait_counter + 32'b1;
                    end
                end
                STATE_PRECHARGE: begin
                    if (wait_counter <= T_RP) begin
                        state        <= STATE_REFRESH;
                        command      <= CMD_REFRESH;
                        ref_counter  <= 32'b0;
                        wait_counter <= 32'b0;
                    end else begin
                        state        <= STATE_PRECHARGE;
                        command      <= CMD_NOP;
                        wait_counter <= wait_counter + 32'b1;
                    end
                end
                STATE_REFRESH: begin
                    if (AUTO_REFRESH_TIMES <= ref_counter) begin
                        state        <= STATE_MODE_REG;
                        command      <= CMD_MODE_REG;
                        inter_addr   <= { 3'b0, MODE_REG };
                        ref_counter  <= 32'b0;
                        wait_counter <= 32'b0;
                    end else if (T_RC <= wait_counter) begin
                        state        <= STATE_REFRESH;
                        command      <= CMD_REFRESH;
                        wait_counter <= 32'b0;
                        ref_counter  <= ref_counter + 32'b1;
                    end else begin
                        state        <= STATE_REFRESH;
                        command      <= CMD_NOP;
                        wait_counter <= wait_counter + 32'b1;
                    end
                end
                STATE_MODE_REG: begin
                    if (T_MRD <= wait_counter) begin
                        state         <= STATE_INIT_FIN;
                        command       <= CMD_NOP;
                        wait_counter  <= 32'b0;
                    end else begin
                        state         <= STATE_MODE_REG;
                        command       <= CMD_NOP;
                        wait_counter  <= wait_counter + 32'b1;
                        inter_addr    <= 13'b0;
                    end
                end
                STATE_INIT_FIN: begin
                    state             <= STATE_INIT_FIN;
                    fin               <= 1'b1;
                    command           <= CMD_NOP;
                    wait_counter      <= 32'b0;
                    ref_counter       <= 32'b0;
                end
                default: begin
                    state             <= state;
                end
            endcase
        end
    end

    assign DRAM_ADDR = inter_addr;
    assign { DRAM_CS_N, DRAM_RAS_N, DRAM_CAS_N, DRAM_WE_N } = command;
    assign DRAM_BA = 2'b0;
    assign { DRAM_UDQM, DRAM_LDQM } = 2'b0;
endmodule

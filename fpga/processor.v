module PROCESSOR(
    input wire CLK, RST_X
);
    localparam AWIDTH = 5'd22;

    localparam IDLE  = 3'd0;
    localparam FETCH = 3'd1;
    localparam EXEC  = 3'd2;  // Decode and Exec
    localparam MEM   = 3'd3;
    localparam WB    = 3'd4;

    reg [2:0] state = IDLE, next_state = IDLE;
    
    // Program counter
    reg [31:0] PC;

    // Detect RST_X
    wire reset;

    // SDRAM
    wire sdram_wr_req;
    wire sdram_rd_req;
    wire sdram_wr_ack;
    wire sdram_rd_ack;
    wire [AWIDTH-1:0] sdram_addr;
    wire [31:0] sdram_rd_data;
    wire [31:0] sdram_wr_data;

    // FETCH
    wire [AWIDTH-1:0] fetch_sdram_addr;
    wire              fetch_sdram_rd_req;
    wire [31:0]       instr;
    reg               fetch_req = 1'b0;
    wire              fetch_ack;

    // CORE
    wire [AWIDTH-1:0] core_mem_addr;
    wire [31:0]       core_next_pc;
    wire [31:0]       core_res;
    wire [31:0]       core_wr_mem_data;
    wire              core_wr_regfile;
    wire              core_wr_mem;
    wire              core_rd_mem;
    reg               core_req;

    // MEM
    wire [AWIDTH-1:0] mem_sdram_addr;
    wire [31:0]       mem_sdram_wr_data;
    wire [31:0]       mem_sdram_rd_data;
    wire              mem_sdram_wr_req;
    wire              mem_sdram_rd_req;
    reg               mem_req = 1'b0;
    wire              mem_ack;

    wire         wb_regfile;
    wire [31:0]  wb_regfile_data;

    DETECT_POSEDGE detect_rst_x(
        .CLK(CLK),
        .SIGNAL(RST_X),
        .EDGE(reset)
    );

    INSTR_FETCH #(
        .AWIDTH(AWIDTH)
    ) instr_fetch(
        .CLK(CLK),
        .RST_X(RST_X),
        .pc(PC),
        .instr(instr),
        .fetch_req(fetch_req),
        .fetch_ack(fetch_ack),
        .sdram_addr(fetch_sdram_addr),
        .sdram_rd_ack(sdram_rd_ack),
        .sdram_rd_req(fetch_sdram_rd_req),
        .sdram_rd_data(sdram_rd_data)
    );
   
    CORE #(
        .AWIDTH(AWIDTH)
    ) core(
        .CLK(CLK),
        .RST_X(RST_X),
        .pc(PC),
        
        .req(core_req),

        .instr(instr),
        .iwr_regfile(wb_regfile),
        .wr_regfile_data(wb_regfile_data),
        .owr_regfile(core_wr_regfile),

        .mem_addr(core_mem_addr),
        .wr_mem_data(core_wr_mem_data),
        .wr_mem(core_wr_mem),
        .rd_mem(core_rd_mem),

        .result(core_res),
        .next_pc(core_next_pc)
    );

    MEM_STAGE #(
        .AWIDTH(AWIDTH)
    ) mem(
        .CLK(CLK),
        .RST_X(RST_X),

        .mem_req(mem_req),
        .mem_ack(mem_ack),

        .wr_mem(core_wr_mem),
        .rd_mem(core_rd_mem),
        .mem_addr(core_mem_addr),
        .mem_wr_data(core_wr_mem_data),

        .mem_rd_data(mem_sdram_rd_data),

        .sdram_rd_ack(sdram_rd_ack),
        .sdram_wr_ack(sdram_wr_ack),
        .sdram_rd_data(sdram_rd_data),
        .sdram_rd_req(mem_sdram_rd_req),
        .sdram_wr_req(mem_sdram_wr_req),
        .sdram_addr(mem_sdram_addr),
        .sdram_wr_data(mem_sdram_wr_data)
    );

    assign #1 sdram_wr_req =  (state == MEM   ? mem_sdram_wr_req   : 1'b0);
    assign #1 sdram_rd_req =  (state == FETCH ? fetch_sdram_rd_req :
                               state == MEM   ? mem_sdram_rd_req   : 1'b0);
    assign #1 sdram_wr_data = (state == MEM   ? mem_sdram_wr_data  : 32'b0);
    assign #1 sdram_addr    = (state == FETCH ? fetch_sdram_addr   :
                               state == MEM   ? mem_sdram_addr     : 0);

    SIMPLE_SDRAM #(
        .AWIDTH(AWIDTH),
        .MEM_SIZE(65536*2)
    ) sdram(
        .CLK(CLK),
        .RST_X(RST_X),
        .wr_req(sdram_wr_req),
        .rd_req(sdram_rd_req),
        .wr_data(sdram_wr_data),
        .rd_data(sdram_rd_data),
        .addr(sdram_addr),
        .wr_ack(sdram_wr_ack),
        .rd_ack(sdram_rd_ack)
    );

    ///// WB Stage /////
    assign #1 wb_regfile      = (state == WB && core_wr_regfile);
    assign #1 wb_regfile_data = (core_rd_mem ? mem_sdram_rd_data : core_res);

    ///// State Transition /////
    always @(state or reset or fetch_ack or mem_ack) begin
        case (state)
            IDLE: begin
                if (reset)     next_state <= FETCH;
                else           next_state <= IDLE;
            end
            FETCH: begin
                if (fetch_ack) next_state <= EXEC;
                else           next_state <= FETCH;
            end
            EXEC:              next_state <= MEM;
            MEM: begin
                if (mem_ack)   next_state <= WB;
                else           next_state <= MEM;
            end
            WB:                next_state <= FETCH;
        endcase
    end

    always @(posedge CLK) state <= next_state;

    always @(state) begin
        if (state == FETCH) PC = core_next_pc;
        fetch_req = (state == FETCH);
        core_req  = (state == EXEC);
        mem_req   = (state == MEM);
    end
endmodule

module CORE #(
    parameter AWIDTH = 25
) (
    input wire CLK,
    input wire RST_X,
    input wire [31:0] pc,
    
    ///// REQ /////
    input wire req,

    ///// Instruction /////
    input wire [31:0] instr,

    ///// REGFILE /////
    input  wire        iwr_regfile,
    input  wire [31:0] wr_regfile_data,
    output wire        owr_regfile,

    ///// MEM /////
    output wire [AWIDTH-1:0] mem_addr,
    output wire [31:0]       wr_mem_data,
    output wire              wr_mem,
    output wire              rd_mem,

    ///// Execution Result /////
    output wire [31:0] result,
    output wire [31:0] next_pc
);
    // DECODE
    wire [2:0]  funct3;
    wire [6:0]  funct7;
    wire [4:0]  instr_type;
    wire [4:0]  rs1;
    wire [4:0]  rs2;
    wire [4:0]  rd;
    wire [31:0] imm;

    // REGFILE
    wire [31:0] rrs1;
    wire [31:0] rrs2;

    INSTR_DECODER decoder(
        .instr(instr),
        .rs1(rs1),
        .rs2(rs2),
        .rd(rd),
        .imm(imm),
        .funct3(funct3),
        .funct7(funct7),
        .instr_type(instr_type)
    );

    REGFILE regfile(
        .CLK(CLK),
        .RST_X(RST_X),
        .wr_regfile(iwr_regfile),
        .wr_regfile_data(wr_regfile_data),
        .rs1(rs1),
        .rs2(rs2),
        .rd(rd),
        .rrs1(rrs1),
        .rrs2(rrs2)
    );

    EXEC_STAGE #(
        .AWIDTH(AWIDTH)
    ) exec(
        .CLK(CLK),
        .RST_X(RST_X),

        .exec_req(req),

        .pc(pc),
        .rrs1(rrs1),
        .rrs2(rrs2),
        .imm(imm),
        .instr_type(instr_type),
        .funct3(funct3),
        .funct7(funct7),

        .next_pc(next_pc),
        .result(result),
        .mem_addr(mem_addr),
        .wr_mem_data(wr_mem_data),
        .wr_mem(wr_mem),
        .rd_mem(rd_mem),
        .wr_regfile(owr_regfile)
    );
endmodule

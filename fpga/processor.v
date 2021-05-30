module PROCESSOR(
    input wire clk,
    input wire reset
);
    localparam IDLE   = 4'd0;
    localparam FETCH  = 4'd1;
    localparam EXEC   = 4'd3;
    localparam MEM_WR = 4'd4;
    localparam MEM_RD = 4'd5;
    localparam WB     = 4'd6;

    reg [3:0] state = IDLE;
    
    // Program counter
    reg [31:0] PC;

    // SDRAM
    wire        sdram_wr_req;
    wire        sdram_rd_req;
    wire        sdram_wr_ack;
    wire        sdram_rd_ack;
    wire [31:0] sdram_wr_addr;
    wire [31:0] sdram_rd_addr;
    wire [31:0] sdram_rd_data;
    wire [31:0] sdram_wr_data;

    // FETCH
    wire [31:0] fetch_sdram_rd_addr;
    wire        fetch_sdram_rd_req;
    wire [31:0] instr;
    reg         fetch_req;
    wire        fetch_fin;

    // CORE
    wire [31:0] core_wr_mem_addr;
    wire [31:0] core_wr_mem_data;
    wire [31:0] core_rd_mem_addr;
    wire [31:0] core_next_pc;
    wire [31:0] core_res;
    wire        core_wr_mem_req;
    wire        core_rd_mem_req;
    wire        core_wr_regfile;
    reg         core_req;

    // MEM
    wire [31:0] mem_sdram_wr_addr;
    wire [31:0] mem_sdram_wr_data;
    wire        mem_sdram_wr_req;
    wire [31:0] mem_sdram_rd_addr;
    wire [31:0] mem_rd_data;
    wire        mem_sdram_rd_req;
    reg         mem_wr_req;
    wire        mem_wr_fin;
    reg         mem_rd_req;
    wire        mem_rd_fin;

    // WB
    reg         wb_wr_regfile;
    reg [31:0]  wb_wr_regfile_data;

    reg start = 0, init = 0;
    always @(posedge reset) start <= 1'b1;

    always @(posedge clk or posedge reset) begin
        if (reset) begin
            state <= #1 IDLE;
        end else if (start) begin
            case (state)
                IDLE: begin
                    state          <= #1 FETCH;
                    fetch_req      <= #1 1'b1;
                    if (~init) begin
                        init       <= 1'b1;
                        PC         <= PC;
                    end else begin
                        PC         <= core_next_pc;
                    end
                end
                FETCH: begin
                    if (fetch_fin) begin
                        fetch_req  <= #1 1'b0;
                        state      <= #1 EXEC;
                        core_req   <= #1 1'b1;
                    end else begin
                        state      <= #1 FETCH;
                    end
                end
                EXEC: begin
                    core_req       <= #1 1'b0;
                    if (core_wr_mem_req) begin
                        state      <= #1 MEM_WR;
                        mem_wr_req <= #1 1'b1;
                    end else if (core_rd_mem_req) begin
                        state      <= #1 MEM_RD;
                        mem_rd_req <= #1 1'b1;
                    end else begin
                        state           <= #1 WB;
                        wb_wr_regfile   <= #1 core_wr_regfile;
                        wb_wr_regfile_data <= #1 core_res;
                    end
                end
                MEM_WR: begin
                    if (mem_wr_fin) begin
                        state      <= #1 IDLE;
                        mem_wr_req <= #1 1'b0;
                    end else begin
                        state      <= #1 MEM_WR;
                    end
                end
                MEM_RD: begin
                    if (mem_rd_fin) begin
                        state           <= #1 WB;
                        mem_rd_req      <= #1 1'b0;
                        wb_wr_regfile   <= #1 1'b1;
                        wb_wr_regfile_data <= #1 mem_rd_data;
                    end else begin
                        state           <= #1 MEM_RD;
                    end
                end
                WB: begin
                    wb_wr_regfile       <= #1 1'b0;
                    state               <= #1 IDLE;
                end
                default: begin
                    state               <= #1 state;
                end
            endcase
        end
    end


    INSTR_FETCH instr_fetch(
        .clk(clk),
        .reset(reset),
        .pc(PC),

        .instr(instr),

        .req(fetch_req),
        .fin(fetch_fin),

        .sdram_rd_req(fetch_sdram_rd_req),
        .sdram_rd_fin(sdram_rd_fin),
        .sdram_rd_addr(fetch_sdram_rd_addr),
        .sdram_rd_data(sdram_rd_data)
    );
   
    CORE core(
        .clk(clk),
        .reset(reset),
        .pc(PC),
        
        .req(core_req),

        .instr(instr),

        .iwr_regfile(wb_wr_regfile),
        .wr_regfile_data(wb_wr_regfile_data),

        .owr_regfile(core_wr_regfile),

        .wr_mem_req(core_wr_mem_req),
        .wr_mem_addr(core_wr_mem_addr),
        .wr_mem_data(core_wr_mem_data),

        .rd_mem_req(core_rd_mem_req),
        .rd_mem_addr(core_rd_mem_addr),

        .result(core_res),
        .next_pc(core_next_pc)
    );

    MEM_STAGE mem(
        .clk(clk),
        .reset(reset),

        .mem_wr_req(mem_wr_req),
        .mem_wr_fin(mem_wr_fin),
        .mem_rd_req(mem_rd_req),
        .mem_rd_fin(mem_rd_fin),

        .mem_wr_addr(core_wr_mem_addr),
        .mem_wr_data(core_wr_mem_data),
        .mem_rd_addr(core_rd_mem_addr),
        .mem_rd_data(mem_rd_data),

        ///// SDRAM /////
        .sdram_rd_fin(sdram_rd_fin),
        .sdram_wr_fin(sdram_wr_fin),
        .sdram_rd_data(sdram_rd_data),

        .sdram_rd_req(mem_sdram_rd_req),
        .sdram_rd_addr(mem_sdram_rd_addr),

        .sdram_wr_req(mem_sdram_wr_req),
        .sdram_wr_addr(mem_sdram_wr_addr),
        .sdram_wr_data(mem_sdram_wr_data)
    );

    ///// WB Stage /////
    assign #1 wb_regfile      = (state == WB && core_wr_regfile);
    assign #1 wb_regfile_data = (core_rd_mem_req ? mem_rd_data : core_res);

    assign #1 sdram_wr_req  = (state == MEM_WR ? mem_sdram_wr_req    : 1'b0);
    assign #1 sdram_wr_addr = (state == MEM_WR ? mem_sdram_wr_addr   : 32'b0);
    assign #1 sdram_wr_data = (state == MEM_WR ? mem_sdram_wr_data   : 32'b0);
    assign #1 sdram_rd_req  = (state == FETCH  ? fetch_sdram_rd_req  :
                               state == MEM_RD ? mem_sdram_rd_req    : 1'b0);
    assign #1 sdram_rd_addr = (state == FETCH  ? fetch_sdram_rd_addr :
                               state == MEM_RD ? mem_sdram_rd_addr   : 32'b0);

    SIMPLE_SDRAM #(
        .MEM_SIZE(65536*2)
    ) sdram(
        .clk(clk),
        .reset(reset),

        .wr_req(sdram_wr_req),
        .wr_fin(sdram_wr_fin),
        .wr_addr(sdram_wr_addr),
        .wr_data(sdram_wr_data),

        .rd_req(sdram_rd_req),
        .rd_fin(sdram_rd_fin),
        .rd_addr(sdram_rd_addr),
        .rd_data(sdram_rd_data)
    );
endmodule

module CORE(
    input wire        clk,
    input wire        reset,
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
    output wire        wr_mem_req,
    output wire [31:0] wr_mem_addr,
    output wire [31:0] wr_mem_data,
    output wire        rd_mem_req,
    output wire [31:0] rd_mem_addr,

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
        .clk(clk),
        .reset(reset),
        .wr_req(iwr_regfile),
        .wr_data(wr_regfile_data),
        .rs1(rs1),
        .rs2(rs2),
        .rd(rd),
        .rrs1(rrs1),
        .rrs2(rrs2)
    );

    EXEC_STAGE exec(
        .clk(clk),
        .reset(reset),

        .req(req),

        .pc(pc),
        .rrs1(rrs1),
        .rrs2(rrs2),
        .imm(imm),
        .instr_type(instr_type),
        .funct3(funct3),
        .funct7(funct7),

        .next_pc(next_pc),
        .result(result),

        .wr_mem(wr_mem_req),
        .wr_mem_addr(wr_mem_addr),
        .wr_mem_data(wr_mem_data),

        .rd_mem(rd_mem_req),
        .rd_mem_addr(rd_mem_addr),
        .wr_regfile(owr_regfile)
    );
endmodule

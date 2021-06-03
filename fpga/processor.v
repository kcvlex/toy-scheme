`default_nettype none

module PROCESSOR(
    input wire clk,
    input wire reset,

    output reg [3:0]  state,
    output reg [31:0] PC,

    input wire req,

    input  wire [31:0] entry_addr,

    output reg         dram_wr_req,
    input  wire        dram_wr_fin,
    output reg  [31:0] dram_wr_addr,
    output reg  [31:0] dram_wr_data,

    output reg         dram_rd_req,
    input  wire        dram_rd_fin,
    output reg  [31:0] dram_rd_addr,
    input  wire [31:0] dram_rd_data,

    output reg  [31:0] display_data,
    output reg         display
);
    localparam IDLE    = 4'd0;
    localparam IF      = 4'd1;
    localparam ID      = 4'd2;
    localparam EX      = 4'd3;
    localparam MEM     = 4'd4;
    localparam WB      = 4'd5;
    localparam INVALID = 4'd15;

    reg [31:0] NEXT_PC;
    
    ////////////////////////////// Regiser //////////////////////////////

    reg [31:0] IF_ID_instr;

    reg [31:0] ID_EX_rrs1;
    reg [31:0] ID_EX_rrs2;
    reg [31:0] ID_EX_imm;
    reg [4:0]  ID_EX_instr_type;
    reg [2:0]  ID_EX_funct3;
    reg [6:0]  ID_EX_funct7;
    reg [4:0]  ID_WB_rd;

    reg        EX_MEM_wr_req;
    reg [31:0] EX_MEM_wr_addr;
    reg [31:0] EX_MEM_wr_data;
    reg        EX_MEM_rd_req;
    reg [31:0] EX_MEM_rd_addr;
    reg        EX_WB_wr_regfile;
    reg        EX_WB_load;
    reg [31:0] EX_WB_data;

    reg [31:0] MEM_WB_data;

    reg        WB_ID_wr_regfile;
    reg [4:0]  WB_ID_rd;
    reg [31:0] WB_ID_wr_data;

    ////////////////////////////// IF //////////////////////////////

    wire [31:0] IF_dram_rd_addr;
    wire        IF_dram_rd_req;
    wire [31:0] IF_instr;
    reg         IF_req;
    wire        IF_fin;

    INSTR_FETCH if_inst(
        .clk(clk),
        .reset(reset),
        .pc(PC),

        .req(IF_req),
        .fin(IF_fin),
        .instr(IF_instr),

        .dram_rd_req(IF_dram_rd_req),
        .dram_rd_fin(dram_rd_fin),
        .dram_rd_addr(IF_dram_rd_addr),
        .dram_rd_data(dram_rd_data)
    );
    

    ////////////////////////////// ID //////////////////////////////

    wire [2:0]  ID_funct3;
    wire [6:0]  ID_funct7;
    wire [4:0]  ID_instr_type;
    wire [4:0]  ID_rs1;
    wire [4:0]  ID_rs2;
    wire [4:0]  ID_rd;
    wire [31:0] ID_imm;
    wire [31:0] ID_rrs1;
    wire [31:0] ID_rrs2;
    wire [31:0] ID_a10;

    INSTR_DECODER id_inst(
        .clk(clk),
        .reset(reset),
        
        .instr(IF_ID_instr),
        .rs1(ID_rs1),
        .rs2(ID_rs2),
        .rd(ID_rd),
        .imm(ID_imm),
        .funct3(ID_funct3),
        .funct7(ID_funct7),
        .instr_type(ID_instr_type)
    );
   
    REGFILE regfile(
        .clk(clk),
        .reset(reset),
        
        .wr_req(WB_ID_wr_regfile),
        .wr_data(WB_ID_wr_data),
        
        .rs1(ID_rs1),
        .rs2(ID_rs2),
        .rd(WB_ID_rd),

        .rrs1(ID_rrs1),
        .rrs2(ID_rrs2),
        .a10(ID_a10)
    );

    ////////////////////////////// EX //////////////////////////////
    
    reg         EX_req;
    wire [31:0] EX_next_pc;
    wire [31:0] EX_result;
    wire        EX_mem_wr_req;
    wire [31:0] EX_mem_wr_addr;
    wire [31:0] EX_mem_wr_data;
    wire        EX_mem_rd_req;
    wire [31:0] EX_mem_rd_addr;
    wire        EX_wr_regfile;
    
    EXEC_STAGE exec(
        .clk(clk),
        .reset(reset),

        .req(EX_req),

        .pc(PC),
        .rrs1(ID_EX_rrs1),
        .rrs2(ID_EX_rrs2),
        .imm(ID_EX_imm),
        .instr_type(ID_EX_instr_type),
        .funct3(ID_EX_funct3),
        .funct7(ID_EX_funct7),

        .next_pc(EX_next_pc),
        .result(EX_result),

        .wr_mem(EX_mem_wr_req),
        .wr_mem_addr(EX_mem_wr_addr),
        .wr_mem_data(EX_mem_wr_data),

        .rd_mem(EX_mem_rd_req),
        .rd_mem_addr(EX_mem_rd_addr),
        .wr_regfile(EX_wr_regfile)
    );
    

    ////////////////////////////// MEM //////////////////////////////

    wire        MEM_wr_fin;
    wire        MEM_rd_fin;
    wire [31:0] MEM_rd_data;
    wire        MEM_fin;
    wire        MEM_dram_wr_req;
    wire [31:0] MEM_dram_wr_addr;
    wire [31:0] MEM_dram_wr_data;
    wire        MEM_dram_rd_req;
    wire [31:0] MEM_dram_rd_addr;
    
    MEM_STAGE mem(
        .clk(clk),
        .reset(reset),

        .mem_wr_req(EX_MEM_wr_req),
        .mem_wr_fin(MEM_wr_fin),
        .mem_wr_addr(EX_MEM_wr_addr),
        .mem_wr_data(EX_MEM_wr_data),
        
        .mem_rd_req(EX_MEM_rd_req),
        .mem_rd_fin(MEM_rd_fin),
        .mem_rd_addr(EX_MEM_rd_addr),
        .mem_rd_data(MEM_rd_data),

        ///// SDRAM /////
        .sdram_wr_req(MEM_dram_wr_req),
        .sdram_wr_fin(dram_wr_fin),
        .sdram_wr_addr(MEM_dram_wr_addr),
        .sdram_wr_data(MEM_dram_wr_data),

        .sdram_rd_req(MEM_dram_rd_req),
        .sdram_rd_fin(dram_rd_fin),
        .sdram_rd_addr(MEM_dram_rd_addr),
        .sdram_rd_data(dram_rd_data)
    );

    assign #1 MEM_fin = MEM_wr_fin | MEM_rd_fin;

    ////////////////////////////// WB //////////////////////////////

    wire        WB_wr_regfile;
    wire [4:0]  WB_rd;
    wire [31:0] WB_wr_data;

    assign #1 WB_rd         = (state == WB    ? ID_WB_rd         : 5'b0);
    assign #1 WB_wr_regfile = (state == WB    ? EX_WB_wr_regfile : 1'b0);
    assign #1 WB_wr_data    = (state != WB    ? 32'b0            :
                               ~WB_wr_regfile ? 32'b0            :
                               EX_WB_load     ? MEM_WB_data      : EX_WB_data);



    ////////////////////////////// SDRAM //////////////////////////////


    ////////////////////////////// STATE TRANSITION //////////////////////////////

    reg init_pc;
    reg mem_init;
    reg [1:0] idle_wait = 2'b0;
    localparam DISPLAY_ADDR = 32'd65864;
    always @(negedge clk or posedge reset) begin
        if (reset) begin
            idle_wait <= 2'b0;
            init_pc          <= #1 1'b0;
            PC               <= #1 entry_addr;
            state            <= #1 IDLE;

            IF_ID_instr      <= #1 32'b0;

            ID_EX_rrs1       <= #1 32'b0;
            ID_EX_rrs2       <= #1 32'b0;
            ID_EX_imm        <= #1 32'b0;
            ID_EX_instr_type <= #1 5'b0;
            ID_EX_funct3     <= #1 3'b0;
            ID_EX_funct7     <= #1 7'b0;
            ID_WB_rd         <= #1 5'b0;

            EX_MEM_wr_req    <= #1 1'b0;
            EX_MEM_wr_addr   <= #1 32'b0;
            EX_MEM_wr_data   <= #1 32'b0;
            EX_MEM_rd_req    <= #1 1'b0;
            EX_MEM_rd_addr   <= #1 32'b0;
            EX_WB_wr_regfile <= #1 1'b0;
            EX_WB_load       <= #1 1'b0;
            EX_WB_data       <= #1 32'b0;

            MEM_WB_data      <= #1 32'b0;

            WB_ID_wr_regfile <= #1 1'b0;
            WB_ID_rd         <= #1 5'b0;
            WB_ID_wr_data    <= #1 32'b0;
            
            display <= 1'b0;
            display_data <= 32'b0;
            dram_wr_req <= 1'b0;
            dram_wr_addr <= 32'b0;
            dram_wr_data <= 32'b0;
            dram_rd_req <= 1'b0;
            dram_rd_addr <= 32'b0;
        end else if (PC == 32'b0) begin
            state            <= #1 IDLE;

            IF_ID_instr      <= #1 32'b0;

            ID_EX_rrs1       <= #1 32'b0;
            ID_EX_rrs2       <= #1 32'b0;
            ID_EX_imm        <= #1 32'b0;
            ID_EX_instr_type <= #1 5'b0;
            ID_EX_funct3     <= #1 3'b0;
            ID_EX_funct7     <= #1 7'b0;
            ID_WB_rd         <= #1 5'b0;

            EX_MEM_wr_req    <= #1 1'b0;
            EX_MEM_wr_addr   <= #1 32'b0;
            EX_MEM_wr_data   <= #1 32'b0;
            EX_MEM_rd_req    <= #1 1'b0;
            EX_MEM_rd_addr   <= #1 32'b0;
            EX_WB_wr_regfile <= #1 1'b0;
            EX_WB_load       <= #1 1'b0;
            EX_WB_data       <= #1 32'b0;

            MEM_WB_data      <= #1 32'b0;

            WB_ID_wr_regfile <= #1 1'b0;
            WB_ID_rd         <= #1 5'b0;
            WB_ID_wr_data    <= #1 32'b0;
        end else if (req) begin
            case (state)
                IDLE: begin
                    if (idle_wait == 2'b11) begin
                        state            <= #1 IF;
                        IF_req           <= #1 1'b1;
                    end else begin
                        if (idle_wait == 2'b0) begin
                            if (~init_pc) begin
                                init_pc      <= #1 1'b1;
                            end else begin
                                PC           <= #1 NEXT_PC;
                            end
                            WB_ID_wr_regfile <= #1 1'b0;
                        end
                        idle_wait <= idle_wait + 2'b1;
                    end
                end
                IF: begin
                    if (IF_fin) begin
                        IF_req      <= #1 1'b0;
                        IF_ID_instr <= #1 IF_instr;
                        state       <= #1 ID;
                    end else begin
                        state       <= #1 IF;
                    end
                    dram_rd_req  <= IF_dram_rd_req;
                    dram_rd_addr <= IF_dram_rd_addr;
                end
                ID: begin
                    state            <= #1 EX;
                    EX_req           <= #1 1'b1;
                    ID_EX_rrs1       <= #1 ID_rrs1;
                    ID_EX_rrs2       <= #1 ID_rrs2;
                    ID_EX_imm        <= #1 ID_imm;
                    ID_EX_instr_type <= #1 ID_instr_type;
                    ID_EX_funct3     <= #1 ID_funct3;
                    ID_EX_funct7     <= #1 ID_funct7;
                    ID_WB_rd         <= #1 ID_rd;
                end
                EX: begin
                    if (EX_mem_wr_req | EX_mem_rd_req) begin
                        state            <= #1 MEM;
                        mem_init         <= #1 1'b1;
                        EX_MEM_wr_req    <= #1 EX_mem_wr_req;
                        EX_MEM_wr_addr   <= #1 EX_mem_wr_addr;
                        EX_MEM_wr_data   <= #1 EX_mem_wr_data;
                        EX_MEM_rd_req    <= #1 EX_mem_rd_req;
                        EX_MEM_rd_addr   <= #1 EX_mem_rd_addr;
                    end else begin
                        state            <= #1 WB;
                    end
                    EX_WB_wr_regfile     <= #1 EX_wr_regfile;
                    EX_WB_load           <= #1 EX_mem_rd_req;
                    EX_WB_data           <= #1 EX_result;
                    NEXT_PC              <= #1 EX_next_pc;
                    EX_req               <= #1 1'b0;
                    if (PC == DISPLAY_ADDR) begin
                        display_data <= ID_a10;
                        display <= 1'b1;
                    end
                end
                MEM: begin
                    if (mem_init) begin
                        mem_init     <= #1 1'b0;
                    end else if (MEM_fin) begin
                        state         <= #1 WB;
                        EX_MEM_wr_req <= #1 1'b0;
                        EX_MEM_rd_req <= #1 1'b0;
                        MEM_WB_data   <= #1 MEM_rd_data;
                    end else begin
                        state         <= #1 MEM;
                    end
                    dram_wr_req  <= #1 MEM_dram_wr_req;
                    dram_wr_addr <= #1 MEM_dram_wr_addr;
                    dram_wr_data <= #1 MEM_dram_wr_data;
                    dram_rd_req  <= #1 MEM_dram_rd_req;
                    dram_rd_addr <= #1 MEM_dram_rd_addr;
                end
                WB: begin
                    idle_wait <= 2'b0;
                    state            <= #1 IDLE;
                    WB_ID_wr_regfile <= #1 WB_wr_regfile;
                    WB_ID_rd         <= #1 WB_rd;
                    WB_ID_wr_data    <= #1 WB_wr_data;
                end
                default: begin
                    state            <= #1 INVALID;
                end
            endcase
        end
    end
endmodule

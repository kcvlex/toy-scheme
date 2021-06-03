module SDRAM_CONTROLLER(
    input wire clk,
    input wire reset,

    output wire init_fin,

    input  wire [31:0] rd_addr,
    input  wire        rd_req,
    output reg         rd_fin,
    output wire [31:0] rd_data,
    
    input  wire [31:0] wr_addr,
    input  wire        wr_req,
    output reg         wr_fin,
    input  wire [31:0] wr_data,


    output wire [12:0] DRAM_ADDR,
    output wire [1:0]  DRAM_BA,
    output wire        DRAM_CAS_N,
    output wire        DRAM_CKE,
    output wire        DRAM_CLK,
    output wire        DRAM_CS_N,
    inout  wire [15:0] DRAM_DQ,
    output wire        DRAM_LDQM,
    output wire        DRAM_RAS_N,
    output wire        DRAM_UDQM,
    output wire        DRAM_WE_N
);
    `include "sdram_params.hv"

    localparam REF_INTERVAL = 32'd350; // FIXME
    localparam RW_REQ_TIMES = 2;

    // Common
    reg [3:0]  state = IDLE, target_state = IDLE;
    reg [3:0]  command = CMD_NOP;
    reg [31:0] wait_counter = 32'b0;
    reg [31:0] wait_interval = 32'b0;

    // Refresh
    reg [31:0] since_last_ref = 32'b0;
  
    // Initialize
    // wire init_fin;

    // Read/Write
    reg [31:0] wr_buffer = 32'b0;
    reg [7:0]  rw_rem = 8'b0;
    wire       consume_req;
    wire       wr_fin_edge, rd_fin_edge;
    reg        send_data;

    // Internal
    wire [12:0]  inter_addr;
    wire [1:0]   inter_ba;
    wire         inter_cas_n;
    wire         inter_cs_n;
    wire [15:0]  inter_dq;
    wire         inter_ldqm;
    wire         inter_udqm;
    wire         inter_ras_n;
    wire         inter_we_n;

    reg [12:0]  r_inter_addr;
    reg [1:0]   r_inter_ba;
    reg [1:0]   r_inter_dqm;
    reg [31:0]  inter_rd_data;
    reg         inter_wr_req;
    reg         inter_rd_req;
   
    wire [12:0] init_addr;
    wire [1:0]  init_ba;
    wire        init_cas_n;
    wire        init_cs_n;
    wire        init_ldqm;
    wire        init_udqm;
    wire        init_ras_n;
    wire        init_we_n;

    SDRAM_INITIALIZER sdram_init(
        .clk(clk),
        .reset(reset),
        .fin(init_fin),

        .DRAM_ADDR(init_addr),
        .DRAM_BA(init_ba),
        .DRAM_CAS_N(init_cas_n),
        .DRAM_CS_N(init_cs_n),
        .DRAM_LDQM(init_ldqm),
        .DRAM_UDQM(init_udqm),
        .DRAM_RAS_N(init_ras_n),
        .DRAM_WE_N(init_we_n)
    );

    always @(posedge clk or posedge reset) begin
        if (reset) begin
            since_last_ref <= 32'b0;
            state          <= PREPARING;
            wait_interval  <= 32'b0;
            target_state   <= IDLE;
            wr_fin         <= 1'b0;
            rd_fin         <= 1'b0;
            wait_counter   <= 32'b0;
            send_data      <= 1'b0;
            inter_wr_req   <= 1'b0;
            inter_rd_req   <= 1'b0;
        end else if (~init_fin) begin
            since_last_ref <= 32'b0;
            state          <= PREPARING;
            wr_fin         <= 1'b0;
            rd_fin         <= 1'b0;
            wait_counter   <= 32'b0;
            send_data      <= 1'b0;
            inter_wr_req   <= 1'b0;
            inter_rd_req   <= 1'b0;
        end else begin
            case (state)
                PREPARING: begin
                    state              <= IDLE;
                    command            <= CMD_NOP;
                    wait_counter       <= 32'b0;
                    since_last_ref     <= 32'b0;
                    r_inter_addr       <= 13'b0;
                    r_inter_ba         <= 2'b0;
                end
                IDLE: begin
                    if (REF_INTERVAL < since_last_ref) begin
                        state          <= WAIT;
                        target_state   <= IDLE;
                        wait_interval  <= T_RC;
                        command        <= CMD_REFRESH;
                        wait_counter   <= 32'b0;
                        since_last_ref <= 32'b0;
                        r_inter_addr   <= 13'b0;
                        r_inter_ba     <= 2'b0;
                        wr_fin         <= wr_req & wr_fin;
                        rd_fin         <= rd_req & rd_fin;
                    end else if (wr_req & ~wr_fin) begin
                        state          <= WAIT;
                        wait_interval  <= T_RCD;
                        target_state   <= ROW_ACTIVE;
                        command        <= CMD_ACT;
                        wait_counter   <= 32'b0;
                        since_last_ref <= since_last_ref + 32'b1;
                        r_inter_addr   <= wr_addr[22:10];
                        r_inter_ba     <= wr_addr[24:23];
                        inter_wr_req   <= 1'b1;
                    end else if (rd_req & ~rd_fin) begin
                        state          <= WAIT;
                        wait_interval  <= T_RCD;
                        target_state   <= ROW_ACTIVE;
                        command        <= CMD_ACT;
                        wait_counter   <= 32'b0;
                        since_last_ref <= since_last_ref + 32'b1;
                        r_inter_addr   <= rd_addr[22:10];
                        r_inter_ba     <= rd_addr[24:23];
                        inter_rd_req   <= 1'b1;
                    end else begin
                        state          <= IDLE;
                        command        <= CMD_NOP;
                        wait_counter   <= 32'b0;
                        since_last_ref <= since_last_ref + 32'b1;
                        r_inter_addr   <= 13'b0;
                        r_inter_ba     <= 2'b0;
                        wr_fin         <= wr_req & wr_fin;
                        rd_fin         <= rd_req & rd_fin;
                    end
                    r_inter_dqm        <= 2'b11;
                end
                ROW_ACTIVE: begin
                    if (inter_wr_req) begin
                        state          <= WRITE;
                        command        <= CMD_WRITE;
                        wr_buffer      <= wr_data;
                        send_data      <= 1'b1;
                        r_inter_addr   <= { 3'b0, wr_addr[9:0] };
                        r_inter_ba     <= wr_addr[24:23];
                        r_inter_dqm    <= 2'b00;
                        rw_rem         <= RW_REQ_TIMES - 1;
                        inter_wr_req   <= 1'b0;
                    end else if (inter_rd_req) begin
                        state          <= READ;
                        command        <= CMD_READ;
                        r_inter_addr   <= { 3'b0, rd_addr[9:0] };
                        r_inter_ba     <= rd_addr[24:23];
                        r_inter_dqm    <= 2'b00;
                        rw_rem         <= RW_REQ_TIMES;
                        inter_rd_req   <= 1'b0;
                    end else begin
                        // FIXME : unreachable ??
                    end
                    since_last_ref     <= since_last_ref + 32'b1;
                end
                WRITE: begin
                    wr_buffer          <= wr_buffer >> 16;
                    command            <= CMD_NOP;
                    if (rw_rem == 0) begin
                        r_inter_dqm    <= 2'b11;
                        state          <= WAIT;
                        target_state   <= PRECHARGE;
                        wait_interval  <= T_RAS;
                        wr_fin         <= 1'b1;
                        send_data      <= 1'b0;
                    end else begin
                        state          <= WRITE;
                        r_inter_dqm    <= 2'b00;
                        send_data      <= 1'b1;
                    end
                    rw_rem             <= rw_rem - 1;
                    since_last_ref     <= since_last_ref + 32'b1;
                end
                READ: begin
                    state              <= WAIT;
                    target_state       <= READING;
                    command            <= CMD_NOP;
                    wait_interval      <= T_CAC - 1;
                    wait_counter       <= 32'b0;
                    since_last_ref     <= since_last_ref + 32'b1;
                end
                READING: begin
                    command            <= CMD_NOP;
                    if (rw_rem == 1) begin
                        state          <= WAIT;
                        target_state   <= PRECHARGE;
                        wait_interval  <= T_RAS;
                        wait_counter   <= 32'b0;
                        r_inter_dqm    <= 2'b00;
                        rd_fin         <= 1'b1;
                    end else begin
                        state          <= READING;
                    end
                    rw_rem             <= rw_rem - 1;
                    since_last_ref     <= since_last_ref + 32'b1;
                end
                PRECHARGE: begin
                    state              <= WAIT;
                    target_state       <= IDLE;
                    wait_interval      <= T_RP;
                    command            <= CMD_PRECHARGE;
                    wait_counter       <= 32'b0;
                    since_last_ref     <= since_last_ref + 32'b1;
                    wr_fin             <= wr_req & wr_fin;
                    rd_fin             <= rd_req & rd_fin;
                end
                WAIT: begin
                    command            <= CMD_NOP;
                    if (wait_counter + 1 == wait_interval) begin
                        state          <= target_state;
                    end else begin
                        state          <= WAIT;
                    end
                    wait_counter       <= wait_counter + 32'b1;
                    since_last_ref     <= since_last_ref + 32'b1;
                    if (target_state == IDLE | target_state == PRECHARGE) begin
                        wr_fin         <= wr_req & wr_fin;
                        rd_fin         <= rd_req & rd_fin;
                    end
                end
                default: begin
                    command            <= CMD_NOP;
                    since_last_ref     <= since_last_ref + 32'b1;
                end
            endcase
        end
    end

    always @(negedge clk) begin
        case (state)
            ROW_ACTIVE: inter_rd_data <= 32'b0;
            READING:    inter_rd_data <= (inter_rd_data << 16) | { 16'b0, DRAM_DQ };
            default:    inter_rd_data <= inter_rd_data;
        endcase
    end

    assign { inter_cs_n, inter_ras_n, inter_cas_n, inter_we_n } = command;
    assign { inter_ldqm, inter_udqm } = r_inter_dqm;
    assign inter_dq = wr_buffer[15:0];
    assign inter_addr = r_inter_addr;
    assign inter_ba = r_inter_ba;

    assign DRAM_ADDR  = (init_fin  ? inter_addr  : init_addr);
    assign DRAM_BA    = (init_fin  ? inter_ba    : init_ba);
    assign DRAM_CAS_N = (init_fin  ? inter_cas_n : init_cas_n);
    assign DRAM_CKE   = 1'b1;
    assign DRAM_CLK   = ~clk;
    assign DRAM_CS_N  = (init_fin  ? inter_cs_n  : init_cs_n);
    assign DRAM_DQ    = (send_data ? inter_dq    : 16'bz);
    assign DRAM_LDQM  = (init_fin  ? inter_ldqm  : init_ldqm);
    assign DRAM_UDQM  = (init_fin  ? inter_udqm  : init_udqm);
    assign DRAM_RAS_N = (init_fin  ? inter_ras_n : init_ras_n);
    assign DRAM_WE_N  = (init_fin  ? inter_we_n  : init_we_n);

    assign rd_data = { inter_rd_data[15:0], inter_rd_data[31:16] };
endmodule

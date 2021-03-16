#ifndef INCLUDE_SIMULATOR_INSTR
#define INCLUDE_SIMULATOR_INSTR

#include "alias.hpp"

namespace simulator {

#if 0
struct Instruction {
    const opcode_type &op = op_;
    const reg_opt_type &rs1 = rs1_;
    const reg_opt_type &rs2 = rs2_;
    const reg_opt_type &rd = rd_;
    const imm_opt_type &imm = imm_;

    Instruction() = delete;

    constexpr Instruction(const opcode_type op_arg,
                          const reg_opt_type rs1_arg,
                          const reg_opt_type rs2_arg,
                          const reg_opt_type rd_arg,
                          const imm_opt_type imm_arg)
        : op_(op_arg),
          rs1_(rs1_arg),
          rs2_(rs2_arg),
          rd_(rd_arg),
          imm_(imm_arg)
    {
    }

    constexpr Instruction& operator=(const Instruction &rhs) {
        op_  = rhs.op_;
        rs1_ = rhs.rs1_;
        rs2_ = rhs.rs2_;
        rd_  = rhs.rd_;
        imm_ = rhs.imm_;
        return *this;
    }

private:
    opcode_type op_;
    reg_opt_type rs1_, rs2_, rd_;
    imm_opt_type imm_;
};

#else

struct Instruction {
    opcode_type op;
    reg_opt_type rs1, rs2, rd;
    imm_opt_type imm;

    constexpr Instruction(const opcode_type op_arg,
                          const reg_opt_type rs1_arg,
                          const reg_opt_type rs2_arg,
                          const reg_opt_type rd_arg,
                          const imm_opt_type imm_arg)
        : op(op_arg),
          rs1(rs1_arg),
          rs2(rs2_arg),
          rd(rd_arg),
          imm(imm_arg)
    {
    }
};

#endif

constexpr Instruction make_jal(const reg_type rd, 
                               const imm_value_type imm) 
{
    return Instruction(opcode_type::JAL, std::nullopt, std::nullopt, rd, imm);
}

constexpr Instruction make_jalr(const reg_type rs1, 
                                const reg_type rd,
                                const imm_value_type imm)
{
    return Instruction(opcode_type::JALR, rs1, std::nullopt, rd, imm);
}

constexpr Instruction make_branch(const opcode_type op,
                                  const reg_type rs1,
                                  const reg_type rs2,
                                  const imm_value_type imm)
{
    return Instruction(op, rs1, rs2, std::nullopt, imm);
}

constexpr Instruction make_lw(const opcode_type op,
                              const reg_type rs1,
                              const reg_type rd,
                              const imm_value_type imm)
{
    return Instruction(op, rs1, std::nullopt, rd, imm);
}

constexpr Instruction make_sw(const opcode_type op,
                              const reg_type rs1,
                              const reg_type rs2,
                              const imm_value_type imm)
{
    return Instruction(op, rs1, rs2, std::nullopt, imm);
}

constexpr Instruction make_op_imm(const opcode_type op,
                                  const reg_type rs1,
                                  const reg_type rd,
                                  const imm_value_type imm)
{
    return Instruction(op, rs1, std::nullopt, rd, imm);
}

constexpr Instruction make_op(const opcode_type op,
                              const reg_type rs1,
                              const reg_type rs2,
                              const reg_type rd)
{
    return Instruction(op, rs1, rs2, rd, std::nullopt);
}

// lb <= op <= ub
constexpr bool check_opcode_range(const opcode_type op,
                                  const opcode_type lb,
                                  const opcode_type ub)
{
    const int op_int = int(op), lb_int = int(lb), ub_int = int(ub);
    return lb_int <= op_int && op_int <= ub_int;
}

constexpr bool is_branch_instr(const opcode_type op) {
    return check_opcode_range(op, opcode_type::BEQ, opcode_type::BGEU);
}

constexpr bool is_load_instr(const opcode_type op) {
    return check_opcode_range(op, opcode_type::LB, opcode_type::LHU);
}

constexpr bool is_store_instr(const opcode_type op) {
    return check_opcode_range(op, opcode_type::SB, opcode_type::SW);
}

constexpr bool is_op_imm_instr(const opcode_type op) {
    return check_opcode_range(op, opcode_type::ADDI, opcode_type::SRAI);
}

constexpr bool is_op_instr(const opcode_type op) {
    return check_opcode_range(op, opcode_type::ADD, opcode_type::AND);
}

}

#endif

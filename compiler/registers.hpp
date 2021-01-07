#ifndef INCLUDE_REGISTERS
#define INCLUDE_REGISTERS

#include <cstdint>

namespace compiler {

enum class Reg {
    zero = 0,
    ra,
    sp,
    gp,
    tp,
    t0, t1, t2, t3, t4, t5, t6,
    s0, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11,
    a0, a1, a2, a3, a4, a5, a6, a7,
    Size,
};

constexpr int reg2int(const Reg reg) {
    return static_cast<int>(reg);
}

constexpr Reg int2reg(const int n) {
    return static_cast<Reg>(n);
}

constexpr Reg nth_arg_reg(const int n) {
    return int2reg(reg2int(Reg::a0) + n);
}

constexpr Reg nth_tmp_reg(const int n) {
    return int2reg(reg2int(Reg::t0) + n);
}

constexpr Reg nth_callee_saved_reg(const int n) {
    return int2reg(reg2int(Reg::s0) + n);
}

constexpr std::size_t arg_reg_num = 8;
constexpr std::size_t tmp_reg_num = 7;
constexpr std::size_t callee_saved_reg_num = 12;

constexpr Reg bp_reg = Reg::s0;
constexpr Reg rv_reg = Reg::a0;

static_assert(reg2int(Reg::Size) == 32, "Invalid Number of Registers");

};

#endif

#include "regfile.hpp"

namespace simulator {

namespace {

/*
 * zero -> x0
 * ra -> x1
 * ...
 */
std::array<std::size_t, 32> mnemonic;

constexpr std::size_t cast_preg2num(const reg_type reg) {
    return static_cast<std::size_t>(reg);
}

constexpr reg_type cast_num2preg(const std::size_t i) {
    return static_cast<reg_type>(i);
}

constexpr reg_type shift_reg(const reg_type reg, const int offset) {
    return cast_num2preg(cast_preg2num(reg) + offset);
}

struct Initializer {
    Initializer() : idx(0) {
        append(reg_type::zero);
        append(reg_type::ra);
        append(reg_type::sp);
        append(reg_type::gp);
        append(reg_type::tp);
        for (std::size_t i = 0; i <= 2; i++)  append(shift_reg(reg_type::t0, i));
        for (std::size_t i = 0; i <= 1; i++)  append(shift_reg(reg_type::s0, i));
        for (std::size_t i = 0; i <= 7; i++)  append(shift_reg(reg_type::a0, i));
        for (std::size_t i = 2; i <= 11; i++) append(shift_reg(reg_type::s0, i));
        for (std::size_t i = 3; i <= 6; i++)  append(shift_reg(reg_type::t0, i));
        build();
    }

private:
    std::size_t idx;
    std::array<reg_type, 32> buf;

    void append(const reg_type reg) {
        buf[idx++] = reg;
    }

    void build() {
        for (std::size_t i = 0; i < 32; i++) {
            mnemonic[cast_preg2num(buf[i])] = i;
        }
    }

} initializer;

/*
 * reg -> x_i, then return i;
 */
std::size_t reg2idx(const reg_type reg) {
    return mnemonic[cast_preg2num(reg)];
}

}

Regfile::Regfile() {
    std::fill(std::next(std::begin(regs)), std::end(regs), std::nullopt);
    regs[reg2idx(reg_type::zero)] = 0;
}


void Regfile::write(const reg_opt_type reg_opt, const value_type dat) {
    if (!reg_opt.has_value()) return;
    const auto reg = *reg_opt;
    if (reg == reg_type::zero) return;
    regs[reg2idx(reg)] = dat;
}

Regfile::value_type Regfile::read(const reg_opt_type reg_opt) const {
    if (!reg_opt.has_value()) return std::nullopt;
    return regs[reg2idx(*reg_opt)];
}

}

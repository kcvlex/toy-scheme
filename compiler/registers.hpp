#ifndef INCLUDE_COMPILER_REGISTERS
#define INCLUDE_COMPILER_REGISTERS

#include <cstdint>

namespace compiler {

enum class PhysicalRegister {
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

constexpr int reg2int(const PhysicalRegister reg) {
    return static_cast<int>(reg);
}

constexpr PhysicalRegister int2reg(const int n) {
    return static_cast<PhysicalRegister>(n);
}

constexpr PhysicalRegister nth_arg_reg(const int n) {
    return int2reg(reg2int(PhysicalRegister::a0) + n);
}

constexpr PhysicalRegister nth_tmp_reg(const int n) {
    return int2reg(reg2int(PhysicalRegister::t0) + n);
}

constexpr PhysicalRegister nth_callee_saved_reg(const int n) {
    return int2reg(reg2int(PhysicalRegister::s0) + n);
}

constexpr std::size_t arg_reg_num = 8;
constexpr std::size_t tmp_reg_num = 7;
constexpr std::size_t callee_saved_reg_num = 12;

constexpr PhysicalRegister bp_reg = PhysicalRegister::s0;
constexpr PhysicalRegister rv_reg = PhysicalRegister::a0;

static_assert(reg2int(PhysicalRegister::Size) == 32, "Invalid Number of PhysicalRegisteristers");


struct VirtualRegister {
    enum class Type {
        StackPtr,
        BasePtr,
        ReturnAddr,
        Arg,
        Normal,
    };

    Type type;
    std::size_t id;

    static VirtualRegister make_normal_reg(const std::size_t id);
    static VirtualRegister make_arg_reg(const std::size_t id);

private:
    VirtualRegister(const Type type_arg,
                    const std::size_t id_arg);
};


};

#endif

#ifndef INCLUDE_COMPILER_THREE_ADDRESS_CODE
#define INCLUDE_COMPILER_THREE_ADDRESS_CODE

#include <utility>
#include <cstdint>
#include <cstddef>
#include <vector>
#include <optional>
#include <variant>
#include <array>
#include <sstream>
#include "alias.hpp"
#include "registers.hpp"

namespace compiler {

using reg_type = std::variant<PhysicalRegister, VirtualRegister>;
using Operand = std::variant<reg_type, 
                             imm_value_type, 
                             label_type>;

enum class Instructions {
    LUI = 0,
    AUIPC,
    JAL, JALR,
    BEQ, BNE, BLT, BGE, BLTU, BGEU,
    LB, LH, LW, LBU, LHU,
    SB, SH, SW,
    ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI, SRLI,
    ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR, AND,
    FENCE,
    Size,
};

Operand preg2operand(const PhysicalRegister preg);
Operand vreg2operand(const VirtualRegister vreg);
Operand imm2operand(const imm_value_type imm);
Operand label2operand(const label_type label);

struct ThreeAddressCode {
    Instructions instr;
    std::optional<Operand> op1, op2, op3;

    ThreeAddressCode();

    ThreeAddressCode(const Instructions instr);

    ThreeAddressCode(const Instructions instr, 
                     const Operand op1);

    ThreeAddressCode(const Instructions instr, 
                     const Operand op1, 
                     const Operand op2);

    ThreeAddressCode(const Instructions instr, 
                     const Operand op1, 
                     const Operand op2, 
                     const Operand op3);

    /*
    void read(reg_type &r1, reg_type &r2, reg_type &r3) const;
    void read(reg_type &r1, reg_type &r2, imm_value_type &imm) const;
    void read(reg_type &r1, reg_type &r2, label_type &label) const;
    void read(reg_type &r1, label_type &label) const;
    */

    // offset(base) <- src
    static ThreeAddressCode make_sw(const reg_type src,
                                    const reg_type base,
                                    const imm_value_type offset);

    // dst <- offset(base)
    static ThreeAddressCode make_lw(const reg_type dst, 
                                    const reg_type base, 
                                    const imm_value_type offset);

    static ThreeAddressCode make_push(const reg_type src);
    static ThreeAddressCode make_pop(const reg_type dst);
    static ThreeAddressCode make_assign(const reg_type dst, const reg_type src);
    static ThreeAddressCode make_nop();
};

std::ostream& operator<<(std::ostream &os, const reg_type &val);
std::ostream& operator<<(std::ostream &os, const Operand &val);
std::ostream& operator<<(std::ostream &os, const ThreeAddressCode &val);

struct CodeSequence : public std::vector<ThreeAddressCode> {
    using vector<ThreeAddressCode>::vector;

    CodeSequence& append_code(ThreeAddressCode code);
};

}

#endif

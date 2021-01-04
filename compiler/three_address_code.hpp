#ifndef INCLUDE_THREE_ADDRESS_CODE
#define INCLUDE_THREE_ADDRESS_CODE

#include <utility>
#include <cstdint>
#include <cstddef>
#include <vector>
#include <optional>
#include <variant>
#include <array>
#include <sstream>
#include "registers.hpp"

namespace compiler {

using imm_value_type = std::int16_t;
using label_type = std::int32_t;  // FIXME
using Operand = std::variant<Reg, imm_value_type, label_type>;

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

Operand reg2operand(const Reg reg);
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

    void read(Reg &r1, Reg &r2, Reg &r3) const;
    void read(Reg &r1, Reg &r2, imm_value_type &imm) const;
    void read(Reg &r1, Reg &r2, label_type &label) const;
    void read(Reg &r1, label_type &label) const;
};

std::ostream& operator<<(std::ostream &os, const ThreeAddressCode &val);

struct InputCodeStream {
    using buffer_type = std::vector<ThreeAddressCode>;
    using const_itr = buffer_type::const_iterator;

    InputCodeStream(buffer_type buf_arg);

    const_itr get() const;
    void advance();
    void advance(std::size_t s);
    bool finished() const noexcept;
    std::size_t entire_size() const noexcept;

private:
    buffer_type buf;
    const_itr cur;
};

// https://stackoverflow.com/questions/18290523/is-a-default-move-constructor-equivalent-to-a-member-wise-move-constructor
struct OutputCodeStream {
    OutputCodeStream();
    OutputCodeStream(const OutputCodeStream&) = default;
    OutputCodeStream(OutputCodeStream&&) = default;
    OutputCodeStream& operator=(const OutputCodeStream&) = default;
    OutputCodeStream& operator=(OutputCodeStream&&) = default;

    OutputCodeStream& append_code(ThreeAddressCode code);
    OutputCodeStream& append_code(const Instructions instr);
    OutputCodeStream& append_code(const Instructions instr, 
                                  const Operand op1);
    OutputCodeStream& append_code(const Instructions instr, 
                                  const Operand op1, 
                                  const Operand op2);
    OutputCodeStream& append_code(const Instructions instr, 
                                  const Operand op1, 
                                  const Operand op2,
                                  const Operand op3);

    // offset(base) <- src
    OutputCodeStream& append_sw_code(const Reg src, 
                                     const Reg base, 
                                     const imm_value_type offset);

    // dst <- offset(base)
    OutputCodeStream& append_lw_code(const Reg dst, 
                                     const Reg base, 
                                     const imm_value_type offset);

    OutputCodeStream& append_push_code(const Reg src);
    OutputCodeStream& append_pop_code(const Reg dst);
    OutputCodeStream& append_assign_code(const Reg dst, const Reg src);
    OutputCodeStream& append_nop_code();
    OutputCodeStream& concat_stream(const OutputCodeStream &oth);

    void clear();

    InputCodeStream convert();
    std::size_t entire_size() const noexcept;

private:
    std::vector<ThreeAddressCode> buf;
};

}

#endif

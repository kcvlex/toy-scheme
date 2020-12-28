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
using ref_mem_type = std::pair<Reg, imm_value_type>;
using Operand = std::variant<Reg, imm_value_type, ref_mem_type>;

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
Operand refmem2operand(const Reg reg, const imm_value_type offset);

enum class Operation { ADD, SUB, FUNC };

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
};

std::ostream& operator<<(std::ostream &os, const ThreeAddressCode &val);

struct OutputCodeStream {
    using buffer_type = std::vector<ThreeAddressCode>;
    using const_itr = buffer_type::const_iterator;

    OutputCodeStream(buffer_type buf_arg);

    const_itr get() const;
    void advance();
    void advance(std::size_t s);
    bool finished() const noexcept;

private:
    buffer_type buf;
    const_itr cur;
};

// https://stackoverflow.com/questions/18290523/is-a-default-move-constructor-equivalent-to-a-member-wise-move-constructor
struct InputCodeStream {
    InputCodeStream();
    InputCodeStream(const InputCodeStream&) = default;
    InputCodeStream(InputCodeStream&&) = default;
    InputCodeStream& operator=(const InputCodeStream&) = default;
    InputCodeStream& operator=(InputCodeStream&&) = default;

    InputCodeStream& append_code(ThreeAddressCode code);
    InputCodeStream& append_code(const Instructions instr);
    InputCodeStream& append_code(const Instructions instr, 
                                 const Operand op1);
    InputCodeStream& append_code(const Instructions instr, 
                                 const Operand op1, 
                                 const Operand op2);
    InputCodeStream& append_code(const Instructions instr, 
                                 const Operand op1, 
                                 const Operand op2,
                                 const Operand op3);

    // offset(base) <- src
    InputCodeStream& append_sw_code(const Reg src, 
                                    const Reg base, 
                                    const imm_value_type offset);

    // dst <- offset(base)
    InputCodeStream& append_lw_code(const Reg dst, 
                                    const Reg base, 
                                    const imm_value_type offset);

    InputCodeStream& append_push_code(const Reg src);
    InputCodeStream& append_pop_code(const Reg dst);
    InputCodeStream& append_assign_code(const Reg dst, const Reg src);
    InputCodeStream& concat_stream(const InputCodeStream &oth);

    void clear();

    OutputCodeStream convert();

private:
    std::vector<ThreeAddressCode> buf;
};

}

#endif

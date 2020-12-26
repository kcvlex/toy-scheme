#ifndef INCLUDE_THREE_ADDRESS_CODE
#define INCLUDE_THREE_ADDRESS_CODE

#include <utility>
#include <cstdint>
#include <cstddef>
#include <vector>
#include <optional>
#include <variant>
#include <array>

namespace compiler {

using register_id_type = std::uint8_t;
using imm_value_type = std::int16_t;
using ref_mem_type = std::pair<register_id_type, imm_value_type>;
using Operand = std::variant<register_id_type, imm_value_type, ref_mem_type>;

namespace regs {

constexpr std::size_t callee_saved_regs_cnt = 12;
constexpr std::size_t argument_regs_cnt = 8;

namespace internal {

template <typename T, T Lower, T Upper, T Current, T... Seq>
constexpr auto range_seq_aux() {
    if constexpr (Current == Upper) {
        return std::integer_sequence<T, Seq...>{};
    } else {
        return range_seq_aux<T, Lower, Upper, Current + 1, Seq..., Current>();
    }
}

template <typename T, T Lower, T Upper>
constexpr auto range_seq() {
    return range_seq_aux<T, Lower, Upper, Lower>();
}

template <typename T, typename U>
struct concat_seq {
};

template <typename T, T... Values0, T... Values1>
struct concat_seq<std::integer_sequence<T, Values0...>, std::integer_sequence<T, Values1...>> {
    using type = std::integer_sequence<T, Values0..., Values1...>;
};

template <typename T, T Lower0, T Upper0, T Lower1, T Upper1>
struct concat_range_seq {
    using seq0 = decltype(range_seq<T, Lower0, Upper0>());
    using seq1 = decltype(range_seq<T, Lower1, Upper1>());
    using type = typename concat_seq<seq0, seq1>::type;
};

template <typename T>
struct seq2array {
};

template <typename T, T... Values>
struct seq2array<std::integer_sequence<T, Values...>> {
    constexpr static std::array<T, sizeof...(Values)> make() {
        return {{ Values... }};
    }
};

template <register_id_type Lower, register_id_type Upper>
constexpr auto make_array() {
    using seq_type = decltype(range_seq<register_id_type, Lower, Upper>());
    return seq2array<seq_type>::make();
}

template <register_id_type Lower0, register_id_type Upper0,
          register_id_type Lower1, register_id_type Upper1>
constexpr auto make_array2() {
    using seq_type = typename concat_range_seq<register_id_type, Lower0, Upper0, Lower1, Upper1>::type;
    return seq2array<seq_type>::make();
}

}

constexpr register_id_type zero = 0;
constexpr register_id_type ra = 1;
constexpr register_id_type sp = 2;
constexpr register_id_type bp = 8;
constexpr register_id_type rv = 10;
constexpr register_id_type t0 = 5, t1 = 6, t2 = 7;
constexpr auto callee_saved_regs = internal::make_array2<8, 9 + 1, 18, 27 + 1>();
constexpr auto argument_regs = internal::make_array<10, 17 + 1>();

}

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

Operand make_reg(const register_id_type reg_id);
Operand make_imm(const imm_value_type imm);
Operand make_ref_mem(const register_id_type reg_id, const imm_value_type offset);

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
};

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
    InputCodeStream& append_sw_code(const register_id_type src, 
                                    const register_id_type base, 
                                    const imm_value_type offset);

    // dst <- offset(base)
    InputCodeStream& append_lw_code(const register_id_type dst, 
                                    const register_id_type base, 
                                    const imm_value_type offset);

    InputCodeStream& append_push_code(const register_id_type src);
    InputCodeStream& append_pop_code(const register_id_type dst);
    InputCodeStream& concat_stream(const InputCodeStream &oth);

    OutputCodeStream convert();

private:
    std::vector<ThreeAddressCode> buf;
};

}

#endif

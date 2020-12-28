#include "three_address_code.hpp"
#include "util/enum2str.hpp"
#include <tuple>

namespace compiler {

/******************** Operand ********************/

Operand reg2operand(const Reg reg) {
    return Operand(std::in_place_index<0>, reg);
}

Operand imm2operand(const imm_value_type imm) {
    return Operand(std::in_place_index<1>, imm);
}

Operand refmem2operand(const Reg reg, const imm_value_type offset) {
    return Operand(std::in_place_index<2>, std::make_pair(reg, offset));
}


/******************** ThreeAddressCode ********************/

ThreeAddressCode::ThreeAddressCode()
    : instr(Instructions::Size), op1(std::nullopt), op2(std::nullopt), op3(std::nullopt)
{
}

ThreeAddressCode::ThreeAddressCode(const Instructions instr)
    : instr(instr), op1(std::nullopt), op2(std::nullopt), op3(std::nullopt)
{
}

ThreeAddressCode::ThreeAddressCode(const Instructions instr,
                                   const Operand op1)
    : instr(instr), op1(op1), op2(std::nullopt), op3(std::nullopt)
{
}

ThreeAddressCode::ThreeAddressCode(const Instructions instr,
                                   const Operand op1,
                                   const Operand op2)
    : instr(instr), op1(op1), op2(op2), op3(std::nullopt)
{
}

ThreeAddressCode::ThreeAddressCode(const Instructions instr,
                                   const Operand op1,
                                   const Operand op2,
                                   const Operand op3)
    : instr(instr), op1(op1), op2(op2), op3(op3)
{
}

void ThreeAddressCode::read(Reg &r1, Reg &r2, Reg &r3) const {
    r1 = std::get<Reg>(op1.value());
    r2 = std::get<Reg>(op2.value());
    r3 = std::get<Reg>(op3.value());
}

void ThreeAddressCode::read(Reg &r1, Reg &r2, imm_value_type &imm) const {
    try {
        r1 = std::get<Reg>(op1.value());
        r2 = std::get<Reg>(op2.value());
        imm = std::get<imm_value_type>(op3.value());
        return;
    } catch (std::exception&) { }

    r1 = std::get<Reg>(op1.value());
    std::tie(r2, imm) = std::get<ref_mem_type>(op2.value());
}


std::ostream& operator<<(std::ostream &os, const ThreeAddressCode &val) {
    os << util::to_str(val.instr);
    std::string elim = "\t";
    for (auto op : { val.op1, val.op2, val.op3 }) {
        if (!op.has_value()) break;
        os << std::exchange(elim, ",");
        auto value = op.value();
        if (auto p = std::get_if<0>(&value)) {
            os << util::to_str(*p);
        } else if (auto p = std::get_if<1>(&value)) {
            os << int(*p);
        } else if (auto p = std::get_if<2>(&value)) {
            auto [ reg, imm ] = *p;
            os << imm << "(" << util::to_str(reg) << ")";
        }
    }
    return os;
}


/******************** OutputCodeStream ********************/

OutputCodeStream::OutputCodeStream(buffer_type buf_arg)
    : buf(std::move(buf_arg)), cur(std::cbegin(buf))
{
}

OutputCodeStream::const_itr OutputCodeStream::get() const {
    return cur;
}

void OutputCodeStream::advance() {
    ++cur;
}

void OutputCodeStream::advance(std::size_t s) {
    std::advance(cur, s);
}

bool OutputCodeStream::finished() const noexcept {
    return cur == std::cend(buf);
}


/******************** InputCodeStream ********************/

InputCodeStream::InputCodeStream() 
    : buf() 
{
}

InputCodeStream& InputCodeStream::append_code(ThreeAddressCode code) {
    buf.push_back(std::move(code));
    return *this;
}

InputCodeStream& InputCodeStream::append_code(const Instructions instr) {
    buf.emplace_back(instr);
    return *this;
}

InputCodeStream& InputCodeStream::append_code(const Instructions instr,
                                              const Operand op1)
{
    buf.emplace_back(instr, op1);
    return *this;
}

InputCodeStream& InputCodeStream::append_code(const Instructions instr,
                                              const Operand op1,
                                              const Operand op2)
{
    buf.emplace_back(instr, op1, op2);
    return *this;
}

InputCodeStream& InputCodeStream::append_code(const Instructions instr,
                                              const Operand op1,
                                              const Operand op2,
                                              const Operand op3)
{
    buf.emplace_back(instr, op1, op2, op3);
    return *this;
}

InputCodeStream& InputCodeStream::append_lw_code(const Reg dst,
                                                 const Reg base,
                                                 const imm_value_type offset)
{
    return append_code(Instructions::LW,
                       reg2operand(dst),
                       refmem2operand(base, offset));
}

InputCodeStream& InputCodeStream::append_sw_code(const Reg src,
                                                 const Reg base,
                                                 const imm_value_type offset) 
{
    return append_code(Instructions::SW,
                       reg2operand(src),
                       refmem2operand(base, offset));
}

InputCodeStream& InputCodeStream::append_push_code(const Reg src) {
    return this->append_sw_code(src, Reg::sp, 0)
                .append_code(Instructions::ADDI, 
                             reg2operand(Reg::sp), 
                             reg2operand(Reg::sp), 
                             imm2operand(-4));
}

InputCodeStream& InputCodeStream::append_pop_code(const Reg dst) {
    return this->append_lw_code(dst, Reg::sp, 0)
                .append_code(Instructions::ADDI, 
                             reg2operand(Reg::sp), 
                             reg2operand(Reg::sp), 
                             imm2operand(4));
}

InputCodeStream& InputCodeStream::append_assign_code(const Reg dst, const Reg src) {
    return this->append_code(Instructions::OR,
                             reg2operand(dst),
                             reg2operand(src),
                             reg2operand(src));
}

InputCodeStream& InputCodeStream::concat_stream(const InputCodeStream &oth) {
    buf.insert(std::end(buf), std::cbegin(oth.buf), std::cend(oth.buf));
    return *this;
}

void InputCodeStream::clear() {
    buf.clear();
}

OutputCodeStream InputCodeStream::convert() {
    return OutputCodeStream(std::move(buf));
}


}


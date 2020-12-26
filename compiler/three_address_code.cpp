#include "three_address_code.hpp"

namespace compiler {

/******************** Operand ********************/

Operand make_reg(const register_id_type reg_id) {
    return Operand(std::in_place_index<0>, reg_id);
}

Operand make_imm(const imm_value_type imm) {
    return Operand(std::in_place_index<1>, imm);
}

Operand make_ref_mem(const register_id_type reg_id, const imm_value_type offset) {
    return Operand(std::in_place_index<2>, reg_id, offset);
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

InputCodeStream& InputCodeStream::append_lw_code(const register_id_type dst,
                                                 const register_id_type base,
                                                 const imm_value_type offset)
{
    return append_code(Instructions::LW,
                       make_reg(dst),
                       make_ref_mem(base, offset));
}

InputCodeStream& InputCodeStream::append_sw_code(const register_id_type src,
                                                 const register_id_type base,
                                                 const imm_value_type offset) 
{
    return append_code(Instructions::SW,
                       make_reg(src),
                       make_ref_mem(base, offset));
}

InputCodeStream& InputCodeStream::append_push_code(const register_id_type src) {
    return this->append_sw_code(src, regs::sp, 0)
                .append_code(Instructions::ADDI, make_reg(regs::sp), make_reg(regs::sp), make_imm(-4));
}

InputCodeStream& InputCodeStream::append_pop_code(const register_id_type dst) {
    return this->append_lw_code(dst, regs::sp, 0)
                .append_code(Instructions::ADDI, make_reg(regs::sp), make_reg(regs::sp), make_imm(4));
}

InputCodeStream& InputCodeStream::concat_stream(const InputCodeStream &oth) {
    buf.insert(std::end(buf), std::cbegin(oth.buf), std::cend(oth.buf));
    return *this;
}

OutputCodeStream InputCodeStream::convert() {
    return OutputCodeStream(std::move(buf));
}


}


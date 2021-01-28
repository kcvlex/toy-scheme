#include "three_address_code.hpp"
#include "util/enum2str.hpp"
#include <tuple>
#include <algorithm>

namespace compiler {

/******************** Operand ********************/

Operand preg2operand(const PhysicalRegister preg) {
    return Operand(reg_type(preg));
}

Operand vreg2operand(const VritualRegister vreg) {
    return Operand(reg_type(vreg));
}

Operand imm2operand(const imm_value_type imm) {
    return Operand(imm);
}

Operand label2operand(const label_type label) {
    return Operand(label);
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

/*
void ThreeAddressCode::read(Reg &r1, Reg &r2, Reg &r3) const {
    r1 = std::get<Reg>(op1.value());
    r2 = std::get<Reg>(op2.value());
    r3 = std::get<Reg>(op3.value());
}

void ThreeAddressCode::read(Reg &r1, Reg &r2, imm_value_type &imm) const {
    r1 = std::get<Reg>(op1.value());
    r2 = std::get<Reg>(op2.value());
    imm = std::get<imm_value_type>(op3.value());
}

void ThreeAddressCode::read(Reg &r1, Reg &r2, label_type &label) const {
    r1 = std::get<Reg>(op1.value());
    r2 = std::get<Reg>(op2.value());
    label = std::get<label_type>(op3.value());
}

void ThreeAddressCode::read(Reg &r1, label_type &label) const {
    r1 = std::get<Reg>(op1.value());
    label = std::get<label_type>(op2.value());
}
*/

std::ostream& operator<<(std::ostream &os, const reg_type &val) {
    return std::visit([&](const auto &v) { return (os << v); }, 
                      val);
}

std::ostream& operator<<(std::ostream &os, const Operand &val) {
    return std::visit([&](const auto &v) { return (os << v); },
                      val);
}

std::ostream& operator<<(std::ostream &os, const ThreeAddressCode &val) {
    os << util::to_str(val.instr);
    std::string elim = "\t";
    
    if (val.instr == Instructions::SW) {
        return (os << elim 
                   << val.op2.value() << ","
                   << val.op3.value() << "("
                   << val.op1.value() << ")");
    }

    if (val.instr == Instructions::LW) {
        return (os << elim
                   << val.op1.value() << ","
                   << val.op3.value() << "("
                   << val.op2.value() << ")");
    }

    for (const auto &op : { val.op1, val.op2, val.op3 }) {
        if (!op.has_value()) break;
        os << std::exchange(elim, ",")
           << op.value();
    }
    return os;
}


/******************** InputCodeStream ********************/

InputCodeStream::InputCodeStream(buffer_type buf_arg)
    : buf(std::move(buf_arg)), cur(std::cbegin(buf))
{
}

InputCodeStream::const_itr InputCodeStream::get() const {
    return cur;
}

std::size_t InputCodeStream::entire_size() const noexcept {
    return buf.size();
}

void InputCodeStream::advance() {
    ++cur;
}

void InputCodeStream::advance(std::size_t s) {
    std::advance(cur, s);
}

bool InputCodeStream::finished() const noexcept {
    return cur == std::cend(buf);
}


/******************** OutputCodeStream ********************/

OutputCodeStream::OutputCodeStream() 
    : buf()
{
}

OutputCodeStream& OutputCodeStream::append_code(ThreeAddressCode code) {
    buf.push_back(std::move(code));
    return *this;
}

OutputCodeStream& OutputCodeStream::append_code(const Instructions instr) {
    buf.emplace_back(instr);
    return *this;
}

OutputCodeStream& OutputCodeStream::append_code(const Instructions instr,
                                                const Operand op1)
{
    buf.emplace_back(instr, op1);
    return *this;
}

OutputCodeStream& OutputCodeStream::append_code(const Instructions instr,
                                                const Operand op1,
                                                const Operand op2)
{
    buf.emplace_back(instr, op1, op2);
    return *this;
}

OutputCodeStream& OutputCodeStream::append_code(const Instructions instr,
                                                const Operand op1,
                                                const Operand op2,
                                                const Operand op3)
{
    buf.emplace_back(instr, op1, op2, op3);
    return *this;
}

OutputCodeStream& OutputCodeStream::append_lw_code(const Reg dst,
                                                   const Reg base,
                                                   const imm_value_type offset)
{
    return append_code(Instructions::LW,
                       reg2operand(dst),
                       reg2operand(base),
                       imm2operand(offset));
}

OutputCodeStream& OutputCodeStream::append_sw_code(const Reg src,
                                                   const Reg base,
                                                   const imm_value_type offset) 
{
    return append_code(Instructions::SW,
                       reg2operand(base),
                       reg2operand(src),
                       imm2operand(offset));
}

OutputCodeStream& OutputCodeStream::append_push_code(const Reg src) {
    return this->append_sw_code(src, Reg::sp, 0)
                .append_code(Instructions::ADDI, 
                             reg2operand(Reg::sp), 
                             reg2operand(Reg::sp), 
                             imm2operand(-4));
}

OutputCodeStream& OutputCodeStream::append_pop_code(const Reg dst) {
    return this->append_lw_code(dst, Reg::sp, 4)
                .append_code(Instructions::ADDI, 
                             reg2operand(Reg::sp), 
                             reg2operand(Reg::sp), 
                             imm2operand(4));
}

OutputCodeStream& OutputCodeStream::append_assign_code(const Reg dst, const Reg src) {
    return this->append_code(Instructions::OR,
                             reg2operand(dst),
                             reg2operand(src),
                             reg2operand(src));
}

OutputCodeStream& OutputCodeStream::append_nop_code() {
    return this->append_code(Instructions::ADDI,
                             reg2operand(Reg::zero),
                             reg2operand(Reg::zero),
                             imm2operand(0));
}

OutputCodeStream& OutputCodeStream::concat(const OutputCodeStream &oth) {
    buf.insert(std::end(buf), std::cbegin(oth.buf), std::cend(oth.buf));
    return *this;
}

OutputCodeStream& OutputCodeStream::concat(OutputCodeStream &&oth) {
    bool rev = false;
    if (buf.size() < oth.buf.size()) {
        rev = true;
        std::swap(buf, oth.buf);
        std::reverse(std::begin(buf), std::end(buf));
        std::reverse(std::begin(oth.buf), std::end(oth.buf));
    }
    buf.insert(std::end(buf), std::cbegin(oth.buf), std::cend(oth.buf));
    if (rev) std::reverse(std::begin(buf), std::end(buf));
    return *this;
}

void OutputCodeStream::clear() {
    buf.clear();
}

std::size_t OutputCodeStream::entire_size() const noexcept {
    return buf.size();
}

InputCodeStream OutputCodeStream::convert() {
    return InputCodeStream(std::move(buf));
}

OutputCodeStream OutputCodeStream::save_caller_saved_regs(const SimpleRegisterAllocator &reg_alloc) {
    OutputCodeStream res;
    imm_value_type offset = 0;

    auto save_aux = [&] (const Reg reg) {
        res.append_sw_code(reg, Reg::sp, offset);
        offset -= 4;
    };

    save_aux(Reg::ra);
    for (std::size_t i = 0; i != reg_alloc.used_tmp_num(); i++) save_aux(nth_tmp_reg(i));
    for (std::size_t i = 0; i != reg_alloc.arg_num(); i++) save_aux(nth_arg_reg(i));

    res.append_code(Instructions::ADDI,
                    reg2operand(Reg::sp),
                    reg2operand(Reg::sp),
                    imm2operand(offset));

    return res;
}

OutputCodeStream OutputCodeStream::save_callee_saved_regs(const SimpleRegisterAllocator &reg_alloc) {
    OutputCodeStream res;
    imm_value_type offset = 0;

    res.append_assign_code(Reg::t0, Reg::sp);

    auto save_aux = [&] (const Reg reg) {
        res.append_sw_code(reg, Reg::sp, offset);
        offset -= 4;
    };

    save_aux(Reg::sp);
    for (std::size_t i = 0; i != reg_alloc.used_callee_saved_num(); i++) save_aux(nth_callee_saved_reg(i));

    res.append_assign_code(Reg::sp, Reg::t0);
    
    return res;
}

OutputCodeStream OutputCodeStream::restore_caller_saved_regs(const SimpleRegisterAllocator &reg_alloc) {
    OutputCodeStream res;
    imm_value_type offset = 0;
    const imm_value_type reg_cnt = 1 + reg_alloc.used_tmp_num() + reg_alloc.arg_num();

    res.append_code(Instructions::ADDI,
                    reg2operand(Reg::t0),
                    reg2operand(Reg::sp),
                    imm2operand(reg_cnt * 4));

    auto restore_aux = [&] (const Reg reg) {
        res.append_sw_code(reg, Reg::t0, offset);
        offset -= 4;
    };

    restore_aux(Reg::ra);
    for (std::size_t i = 0; i != reg_alloc.used_tmp_num(); i++) restore_aux(nth_tmp_reg(i));
    for (std::size_t i = 0; i != reg_alloc.arg_num(); i++) restore_aux(nth_arg_reg(i));

    res.append_assign_code(Reg::sp, Reg::t0);

    return res;
}

OutputCodeStream OutputCodeStream::restore_callee_saved_regs(const SimpleRegisterAllocator &reg_alloc) {
    OutputCodeStream res;
    imm_value_type offset = 0;

    res.append_assign_code(Reg::t0, bp_reg);

    auto restore_aux = [&] (const Reg reg) {
        res.append_sw_code(reg, Reg::t0, offset);
        offset -= 4;
    };

    restore_aux(Reg::sp);
    for (std::size_t i = 0; i != reg_alloc.used_callee_saved_num(); i++) restore_aux(nth_callee_saved_reg(i));

    return res;
}

}


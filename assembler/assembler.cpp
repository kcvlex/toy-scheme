#include "assembler.hpp"
#include "util/bit_operation.hpp"
#include "util/enum2str.hpp"
#include <algorithm>
#include <cassert>
#include <iostream>

namespace assembler {

Assembler::Assembler(std::vector<compiler::FunctionCode> fcodes_arg)
    : cur_addr(0), 
      fcodes(std::move(fcodes_arg))
{
    line_sum = set_label2addr();
}
    
const std::vector<Assembler::raw_type>& Assembler::encode() {
    encoded.reserve(line_sum);
    for (auto &[ label, os ] : fcodes) {
        auto is = std::move(os.convert());
        while (!is.finished()) {
            const auto c = *is.get();
            is.advance();
            encoded.push_back(encode_three_addr_code(c));
        }
    }
    return encoded;
}

std::size_t Assembler::set_label2addr() {
    label2addr.resize(fcodes.size());
    instr_addr_type sum = 0;
    for (const auto &[ label, codes ] : fcodes) {
        if (label != -1) label2addr[label] = sum * 4;
        sum += codes.entire_size();
    }
    for (auto e : label2addr) std::cout << e << std::endl;
    return sum;
}

Assembler::raw_type Assembler::encode_three_addr_code(const compiler::ThreeAddressCode &code) {
    const auto res = [&] {
        switch (code.instr) {
            case Instructions::JAL:   return encode_jal(code);
            case Instructions::JALR:  return encode_jalr(code);
            case Instructions::LB:    return encode_lb(code);
            case Instructions::LH:    return encode_lh(code);
            case Instructions::LW:    return encode_lw(code);
            case Instructions::SB:    return encode_sb(code);
            case Instructions::SH:    return encode_sh(code);
            case Instructions::SW:    return encode_sw(code);
            case Instructions::ADDI:  return encode_addi(code);
            case Instructions::SLTI:  return encode_slti(code);
            case Instructions::SLTIU: return encode_sltiu(code);
            case Instructions::XORI:  return encode_xori(code);
            case Instructions::ORI:   return encode_ori(code);
            case Instructions::ANDI:  return encode_andi(code);
            // case Instructions::SLLI:  return encode_slli(code);
            // case Instructions::SRLI:  return encode_srli(code);
            case Instructions::ADD:   return encode_add(code);
            case Instructions::SUB:   return encode_sub(code);
            case Instructions::SLL:   return encode_sll(code);
            case Instructions::SLT:   return encode_slt(code);
            case Instructions::SLTU:  return encode_sltu(code);
            case Instructions::XOR:   return encode_xor(code);
            case Instructions::SRL:   return encode_srl(code);
            case Instructions::SRA:   return encode_sra(code);
            case Instructions::OR:    return encode_or(code);
            case Instructions::AND:   return encode_and(code);
            default: assert(false);
        }
    }();
    cur_addr += 4;
    return res;
}

Assembler::raw_type Assembler::encode_jal(const compiler::ThreeAddressCode &code) const {
    compiler::Reg rd;
    compiler::label_type label;
    code.read(rd, label);
    const std::int32_t imm = label2addr[label] - cur_addr;
    return J_type(0b11'011'11,
                  get_reg_id(rd),
                  imm);
}

Assembler::raw_type Assembler::encode_jalr(const compiler::ThreeAddressCode &code) const {
    return I_type_aux(0b11'001'11, 0b000, code);
}

Assembler::raw_type Assembler::encode_lb(const compiler::ThreeAddressCode &code) const {
    return I_type_aux(0b00'000'11, 0b000, code);
}

Assembler::raw_type Assembler::encode_lh(const compiler::ThreeAddressCode &code) const {
    return I_type_aux(0b00'000'11, 0b001, code);
}

Assembler::raw_type Assembler::encode_lw(const compiler::ThreeAddressCode &code) const {
    return I_type_aux(0b00'000'11, 0b010, code);
}
    
Assembler::raw_type Assembler::encode_sb(const compiler::ThreeAddressCode &code) const {
    return S_type_aux(0b01'000'11, 0b000, code);
}
   
Assembler::raw_type Assembler::encode_sh(const compiler::ThreeAddressCode &code) const {
    return S_type_aux(0b01'000'11, 0b001, code);
}
   
Assembler::raw_type Assembler::encode_sw(const compiler::ThreeAddressCode &code) const {
    return S_type_aux(0b01'000'11, 0b010, code);
}

Assembler::raw_type Assembler::encode_addi(const compiler::ThreeAddressCode &code) const {
    return I_type_aux(0b00'100'11, 0b000, code);
}

Assembler::raw_type Assembler::encode_slti(const compiler::ThreeAddressCode &code) const {
    return I_type_aux(0b00'100'11, 0b010, code);
}

Assembler::raw_type Assembler::encode_sltiu(const compiler::ThreeAddressCode &code) const {
    return I_type_aux(0b00'100'11, 0b011, code);
}

Assembler::raw_type Assembler::encode_xori(const compiler::ThreeAddressCode &code) const {
    return I_type_aux(0b00'100'11, 0b100, code);
}

Assembler::raw_type Assembler::encode_ori(const compiler::ThreeAddressCode &code) const {
    return I_type_aux(0b00'100'11, 0b110, code);
}

Assembler::raw_type Assembler::encode_andi(const compiler::ThreeAddressCode &code) const {
    return I_type_aux(0b00'100'11, 0b111, code);
}

Assembler::raw_type Assembler::encode_add(const compiler::ThreeAddressCode &code) const {
    return R_type_aux(0b01'100'11, 0, 0, code);
}

Assembler::raw_type Assembler::encode_sub(const compiler::ThreeAddressCode &code) const {
    return R_type_aux(0b01'100'11, 0, 0b0100000, code);
}

Assembler::raw_type Assembler::encode_sll(const compiler::ThreeAddressCode &code) const {
    return R_type_aux(0b01'100'11, 0b001, 0, code);
}

Assembler::raw_type Assembler::encode_slt(const compiler::ThreeAddressCode &code) const {
    return R_type_aux(0b01'100'11, 0b010, 0, code);
}

Assembler::raw_type Assembler::encode_sltu(const compiler::ThreeAddressCode &code) const {
    return R_type_aux(0b01'100'11, 0b011, 0, code);
}

Assembler::raw_type Assembler::encode_xor(const compiler::ThreeAddressCode &code) const {
    return R_type_aux(0b01'100'11, 0b100, 0, code);
}

Assembler::raw_type Assembler::encode_srl(const compiler::ThreeAddressCode &code) const {
    return R_type_aux(0b01'100'11, 0b101, 0, code);
}

Assembler::raw_type Assembler::encode_sra(const compiler::ThreeAddressCode &code) const {
    return R_type_aux(0b01'100'11, 0b101, 0b0100000, code);
}

Assembler::raw_type Assembler::encode_or(const compiler::ThreeAddressCode &code) const {
    return R_type_aux(0b01'100'11, 0b110, 0, code);
}

Assembler::raw_type Assembler::encode_and(const compiler::ThreeAddressCode &code) const {
    return R_type_aux(0b01'100'11, 0b111, 0, code);
}

using register_id_type = std::uint8_t;

namespace {
    
constexpr std::uint8_t reg_id_sz = 5;
constexpr std::uint8_t funct3_sz = 3;
constexpr std::uint8_t funct7_sz = 7;
constexpr std::uint8_t opcode_sz = 7;

struct raw_instr_builder {
    Assembler::raw_type val = 0;

    raw_instr_builder& push_opcode(const std::int32_t opcode) { val = (val << opcode_sz) + opcode; return *this; }
    raw_instr_builder& push_reg_id(const std::int32_t reg_id) { val = (val << reg_id_sz) + reg_id; return *this; }
    raw_instr_builder& push_funct3(const std::int32_t funct3) { val = (val << funct3_sz) + funct3; return *this; }
    raw_instr_builder& push_funct7(const std::int32_t funct7) { val = (val << funct7_sz) + funct7; return *this; }

    raw_instr_builder& push_imm(const BitOperation &bop, const std::uint8_t r, const std::uint8_t l) {
        auto v = bop(r, l);  // if r or l is ill-formed, error here.
        const auto len = r - l + 1;
        val <<= len;
        val += v.get();
        return *this;
    }
};

using compiler::Reg;

constexpr std::array<compiler::Reg, 32> ordered_regs = {{
    Reg::zero, Reg::ra, Reg::sp, Reg::gp, Reg::tp,
    Reg::t0, Reg::t1, Reg::t2,
    Reg::s0, Reg::s1,
    Reg::a0, Reg::a1, Reg::a2, Reg::a3, Reg::a4, Reg::a5, Reg::a6, Reg::a7,
    Reg::s2, Reg::s3, Reg::s4, Reg::s5, Reg::s6, Reg::s7, Reg::s8, Reg::s9, Reg::s10, Reg::s11,
    Reg::t3, Reg::t4, Reg::t5, Reg::t6,
}};


}  // anonymous

// FIXME
std::int32_t Assembler::get_reg_id(const compiler::Reg reg) const {
    const auto &arr = ordered_regs;
    const auto ite = std::find(std::cbegin(arr), std::cend(arr), reg);
    return std::distance(std::cbegin(arr), ite);
}

Assembler::raw_type Assembler::R_type(const std::int32_t opcode, 
                                      const std::int32_t rs1, 
                                      const std::int32_t rs2, 
                                      const std::int32_t rd,
                                      const std::int32_t funct3,
                                      const std::int32_t funct7) const
{
    raw_instr_builder builder;
    builder.push_funct7(funct7)
           .push_reg_id(rs2)
           .push_reg_id(rs1)
           .push_funct3(funct3)
           .push_reg_id(rd)
           .push_opcode(opcode);
    return builder.val;
}

Assembler::raw_type Assembler::I_type(const std::int32_t opcode,
                                      const std::int32_t rs1,
                                      const std::int32_t rd,
                                      const std::int32_t funct3,
                                      const std::int32_t imm) const
{
    raw_instr_builder builder;
    BitOperation b_imm(imm);
    builder.push_imm(b_imm, 11, 0)
           .push_reg_id(rs1)
           .push_funct3(funct3)
           .push_reg_id(rd)
           .push_opcode(opcode);
    return builder.val;
}

Assembler::raw_type Assembler::S_type(const std::int32_t opcode, 
                                      const std::int32_t rs1,
                                      const std::int32_t rs2,
                                      const std::int32_t funct3,
                                      const std::int32_t imm) const
{
    raw_instr_builder builder;
    BitOperation b_imm(imm);
    builder.push_imm(b_imm, 11, 5)
           .push_reg_id(rs2)
           .push_reg_id(rs1)
           .push_funct3(funct3)
           .push_imm(b_imm, 4, 0)
           .push_opcode(opcode);
    return builder.val;
}
    
Assembler::raw_type Assembler::B_type(const std::int32_t opcode,
                                      const std::int32_t rs1,
                                      const std::int32_t rs2,
                                      const std::int32_t funct3,
                                      const std::int32_t imm) const
{
    raw_instr_builder builder;
    BitOperation b_imm(imm);
    builder.push_imm(b_imm, 10, 5)
           .push_reg_id(rs2)
           .push_reg_id(rs1)
           .push_funct3(funct3)
           .push_imm(b_imm, 4, 1)
           .push_imm(b_imm, 11, 11)
           .push_opcode(opcode);
    return builder.val;
}
    
Assembler::raw_type Assembler::U_type(const std::int32_t opcode,
                                      const std::int32_t rd,
                                      const std::int32_t imm) const
{
    raw_instr_builder builder;
    BitOperation b_imm(imm);
    builder.push_imm(b_imm, 31, 12)
           .push_reg_id(rd)
           .push_opcode(opcode);
    return builder.val;
}

Assembler::raw_type Assembler::J_type(const std::int32_t opcode,
                                      const std::int32_t rd,
                                      const std::int32_t imm) const
{
    std::cout << "imm = " << imm << std::endl;
    raw_instr_builder builder;
    BitOperation b_imm(imm);
    builder.push_imm(b_imm, 20, 20)
           .push_imm(b_imm, 10, 1)
           .push_imm(b_imm, 11, 11)
           .push_imm(b_imm, 19, 12)
           .push_reg_id(rd)
           .push_opcode(opcode);
    return builder.val;
}

Assembler::raw_type Assembler::J_type_aux(const std::int32_t opcode,
                                          const compiler::ThreeAddressCode &code) const
{
    compiler::Reg rd;
    compiler::label_type label;
    code.read(rd, label);
    return J_type(opcode,
                  get_reg_id(rd),
                  label2addr[label]);
}

// FIXME : label
Assembler::raw_type Assembler::U_type_aux(const std::int32_t opcode,
                                          const compiler::ThreeAddressCode &code) const
{
    compiler::Reg rd;
    compiler::label_type label;
    code.read(rd, label);
    return U_type(opcode,
                  get_reg_id(rd),
                  label2addr[label]);
}

Assembler::raw_type Assembler::R_type_aux(const std::int32_t opcode, 
                                          const std::int32_t funct3,
                                          const std::int32_t funct7,
                                          const compiler::ThreeAddressCode &code) const 
{
    compiler::Reg rd, rs1, rs2;
    code.read(rd, rs1, rs2);
    return R_type(opcode,
                  get_reg_id(rs1),
                  get_reg_id(rs2),
                  get_reg_id(rd),
                  funct3,
                  funct7);
}

Assembler::raw_type Assembler::I_type_aux(const std::int32_t opcode, 
                                          const std::int32_t funct3,
                                          const compiler::ThreeAddressCode &code) const 
{
    compiler::Reg rd, rs1;
    compiler::imm_value_type imm;
    code.read(rd, rs1, imm);
    return I_type(opcode,
                  get_reg_id(rs1),
                  get_reg_id(rd),
                  funct3,
                  imm);
}

Assembler::raw_type Assembler::S_type_aux(const std::int32_t opcode,
                                          const std::int32_t funct3,
                                          const compiler::ThreeAddressCode &code) const 
{
    compiler::Reg rs1, rs2;
    compiler::imm_value_type imm;
    code.read(rs1, rs2, imm);
    return S_type(opcode,
                  get_reg_id(rs1),
                  get_reg_id(rs2),
                  funct3,
                  imm);
}

}

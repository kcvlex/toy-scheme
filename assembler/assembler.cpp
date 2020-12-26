#include "assembler.hpp"
#include "bit_operation.hpp"
#include "util/enum2str.hpp"
#include <algorithm>
#include <sstream>
#include <cassert>
#include <iostream>

namespace assembler {

Assembler::raw_type Assembler::encode(const std::string &line) const {
    std::stringstream ss(line);
    std::string s_instr, operands;

    // FIXME : label
    ss >> s_instr;
    ss >> operands;
    assert(ss.eof());

    std::for_each(std::begin(operands), std::end(operands),
                  [&](char &c) {
                      if (c == ',') c = ' ';
                  });

    auto instr = str2instr(s_instr);
    switch (instr) {
        case Instructions::ADDI:  return encode_addi(operands);
        case Instructions::SLTI:  return encode_slti(operands);
        case Instructions::SLTIU: return encode_sltiu(operands);
        case Instructions::XORI:  return encode_xori(operands);
        case Instructions::ORI:   return encode_ori(operands);
        case Instructions::ANDI:  return encode_andi(operands);
        // case Instructions::SLLI:  return encode_slli(operands);
        // case Instructions::SRLI:  return encode_srli(operands);
        case Instructions::ADD:   return encode_add(operands);
        case Instructions::SUB:   return encode_sub(operands);
        case Instructions::SLL:   return encode_sll(operands);
        case Instructions::SLT:   return encode_slt(operands);
        case Instructions::SLTU:  return encode_sltu(operands);
        case Instructions::XOR:   return encode_xor(operands);
        case Instructions::SRL:   return encode_srl(operands);
        case Instructions::SRA:   return encode_sra(operands);
        case Instructions::OR:    return encode_or(operands);
        case Instructions::AND:   return encode_and(operands);
        default: assert(false);
    }
}
    
Assembler::raw_type Assembler::encode_addi(const std::string &operands) const {
    return I_type_aux(0b00'100'11, 0b000, operands);
}

Assembler::raw_type Assembler::encode_slti(const std::string &operands) const {
    return I_type_aux(0b00'100'11, 0b010, operands);
}

Assembler::raw_type Assembler::encode_sltiu(const std::string &operands) const {
    return I_type_aux(0b00'100'11, 0b011, operands);
}

Assembler::raw_type Assembler::encode_xori(const std::string &operands) const {
    return I_type_aux(0b00'100'11, 0b100, operands);
}

Assembler::raw_type Assembler::encode_ori(const std::string &operands) const {
    return I_type_aux(0b00'100'11, 0b110, operands);
}

Assembler::raw_type Assembler::encode_andi(const std::string &operands) const {
    return I_type_aux(0b00'100'11, 0b111, operands);
}

Assembler::raw_type Assembler::encode_add(const std::string &operands) const {
    return R_type_aux(0b01'100'11, 0, 0, operands);
}

Assembler::raw_type Assembler::encode_sub(const std::string &operands) const {
    return R_type_aux(0b01'100'11, 0, 0b0100000, operands);
}

Assembler::raw_type Assembler::encode_sll(const std::string &operands) const {
    return R_type_aux(0b01'100'11, 0b001, 0, operands);
}

Assembler::raw_type Assembler::encode_slt(const std::string &operands) const {
    return R_type_aux(0b01'100'11, 0b010, 0, operands);
}

Assembler::raw_type Assembler::encode_sltu(const std::string &operands) const {
    return R_type_aux(0b01'100'11, 0b011, 0, operands);
}

Assembler::raw_type Assembler::encode_xor(const std::string &operands) const {
    return R_type_aux(0b01'100'11, 0b100, 0, operands);
}

Assembler::raw_type Assembler::encode_srl(const std::string &operands) const {
    return R_type_aux(0b01'100'11, 0b101, 0, operands);
}

Assembler::raw_type Assembler::encode_sra(const std::string &operands) const {
    return R_type_aux(0b01'100'11, 0b101, 0b0100000, operands);
}

Assembler::raw_type Assembler::encode_or(const std::string &operands) const {
    return R_type_aux(0b01'100'11, 0b110, 0, operands);
}

Assembler::raw_type Assembler::encode_and(const std::string &operands) const {
    return R_type_aux(0b01'100'11, 0b111, 0, operands);
}

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

}

std::int32_t Assembler::get_reg_id(const std::string &reg) const {
    if (reg == "zero") return 0;
    if (reg == "ra") return 1;
    if (reg == "sp") return 2;
    if (reg == "gp") return 3;
    if (reg == "tp") return 4;

    if (reg == "t0") return 5;
    if (reg == "t1") return 6;
    if (reg == "t2") return 7;

    if (reg == "s0") return 8;
    if (reg == "s1") return 9;

    for (int i = 10; i <= 17; i++) {
        std::string s;
        s += "a";
        s += std::to_string(i - 10);
        if (reg == s) return i;
    }

    for (int i = 18; i <= 27; i++) {
        std::string s;
        s += "s";
        s += std::to_string(i - 16);
        if (reg == s) return i;
    }

    for (int i = 28; i < 32; i++) {
        std::string s;
        s += "t";
        s += std::to_string(i - 28 + 3);
        if (reg == s) return i;
    }

    assert(false);
}

Instructions Assembler::str2instr(std::string instr_str) const {
    std::for_each(std::begin(instr_str), std::end(instr_str),
                  [&](char &c) {
                      if ('a' <= c && c <= 'z') {
                          c = static_cast<char>(c - 'a' + 'A');
                      } else {
                      }
                  });

    for (std::size_t i = 0; i != std::size_t(Instructions::Size); i++) {
        auto instr = static_cast<Instructions>(i);
        if (util::to_str<Instructions>(instr) == instr_str) return instr;
    }

    std::cout << instr_str << std::endl;
    assert(false);
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

Assembler::raw_type Assembler::R_type_aux(const std::int32_t opcode, 
                                          const std::int32_t funct3,
                                          const std::int32_t funct7,
                                          const std::string &operands) const 
{
    std::stringstream ss(operands);
    std::string rd, rs1, rs2;
    ss >> rd >> rs1 >> rs2;
    return R_type(opcode,
                  get_reg_id(rs1),
                  get_reg_id(rs2),
                  get_reg_id(rd),
                  funct3,
                  funct7);
}

Assembler::raw_type Assembler::I_type_aux(const std::int32_t opcode, 
                                          const std::int32_t funct3,
                                          const std::string &operands) const 
{
    std::stringstream ss(operands);
    std::string rd, rs1;
    std::int32_t imm;
    ss >> rd >> rs1 >> imm;
    return I_type(opcode,
                  get_reg_id(rs1),
                  get_reg_id(rd),
                  funct3,
                  imm);
}

}

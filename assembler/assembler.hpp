#ifndef INCLUDE_ASSEMBLER
#define INCLUDE_ASSEMBLER

#include <vector>
#include <string>
#include "compiler/three_address_code.hpp"

namespace assembler {

using compiler::Instructions;

struct Assembler {
    using raw_type = std::uint32_t;

    raw_type encode(const compiler::ThreeAddressCode &code) const;

private:
    raw_type encode_lb(const compiler::ThreeAddressCode &code) const;
    raw_type encode_lh(const compiler::ThreeAddressCode &code) const;
    raw_type encode_lw(const compiler::ThreeAddressCode &code) const;
    raw_type encode_sb(const compiler::ThreeAddressCode &code) const;
    raw_type encode_sh(const compiler::ThreeAddressCode &code) const;
    raw_type encode_sw(const compiler::ThreeAddressCode &code) const;
    raw_type encode_addi(const compiler::ThreeAddressCode &code) const;
    raw_type encode_slti(const compiler::ThreeAddressCode &code) const;
    raw_type encode_sltiu(const compiler::ThreeAddressCode &code) const;
    raw_type encode_xori(const compiler::ThreeAddressCode &code) const;
    raw_type encode_ori(const compiler::ThreeAddressCode &code) const;
    raw_type encode_andi(const compiler::ThreeAddressCode &code) const;
    // raw_type encode_slli(const compiler::ThreeAddressCode &code) const;
    // raw_type encode_srli(const compiler::ThreeAddressCode &code) const;
    raw_type encode_add(const compiler::ThreeAddressCode &code) const;
    raw_type encode_sub(const compiler::ThreeAddressCode &code) const;
    raw_type encode_sll(const compiler::ThreeAddressCode &code) const;
    raw_type encode_slt(const compiler::ThreeAddressCode &code) const;
    raw_type encode_sltu(const compiler::ThreeAddressCode &code) const;
    raw_type encode_xor(const compiler::ThreeAddressCode &code) const;
    raw_type encode_srl(const compiler::ThreeAddressCode &code) const;
    raw_type encode_sra(const compiler::ThreeAddressCode &code) const;
    raw_type encode_or(const compiler::ThreeAddressCode &code) const;
    raw_type encode_and(const compiler::ThreeAddressCode &code) const;

    // return register index
    std::int32_t get_reg_id(const compiler::Reg reg) const;

    // funct7 | rs2 | rs1 | funct3 | rd | opcode
    raw_type R_type(const std::int32_t opcode, 
                    const std::int32_t rs1, 
                    const std::int32_t rs2, 
                    const std::int32_t rd,
                    const std::int32_t funct3,
                    const std::int32_t funct7) const;

    // imm[11:0] | rs1 | funct3 | rd | opcode
    raw_type I_type(const std::int32_t opcode,
                    const std::int32_t rs1,
                    const std::int32_t rd,
                    const std::int32_t funct3,
                    const std::int32_t imm) const;

    // imm[11:5] | rs2 | rs1 | funct3 | imm[4:0] | opcode
    raw_type S_type(const std::int32_t opcode, 
                    const std::int32_t rs1,
                    const std::int32_t rs2,
                    const std::int32_t funct3,
                    const std::int32_t imm) const;

    // imm[12|10:5] | rs2 | rs1 | funct3 | imm[4:1|11] | opcode
    raw_type B_type(const std::int32_t opcode,
                    const std::int32_t rs1,
                    const std::int32_t rs2,
                    const std::int32_t funct3,
                    const std::int32_t imm) const;

    // imm[31:12] | rd | opcode
    raw_type U_type(const std::int32_t opcode,
                    const std::int32_t rd,
                    const std::int32_t imm) const;

    // imm[20|10:1|11|19:12] | rd | opcode
    raw_type J_type(const std::int32_t opcode,
                    const std::int32_t rd,
                    const std::int32_t imm) const;
    
    raw_type R_type_aux(const std::int32_t opcode, 
                        const std::int32_t funct3,
                        const std::int32_t funct7,
                        const compiler::ThreeAddressCode &code) const;

    raw_type I_type_aux(const std::int32_t opcode,
                        const std::int32_t funct3,
                        const compiler::ThreeAddressCode &code) const;

    raw_type S_type_aux(const std::int32_t opcode,
                        const std::int32_t funct3,
                        const compiler::ThreeAddressCode &code) const;
};

}


#endif

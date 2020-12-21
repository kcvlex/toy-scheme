#ifndef INCLUDE_ASSEMBLER
#define INCLUDE_ASSEMBLER

#include <vector>
#include <string>

namespace assembler {

#define INSTRUCTIONS \
    DUMMY = 0, \
    LUI, \
    AUIPC, \
    JAL, \
    JALR, \
    BEQ, \
    BNE, \
    BLT, \
    BGE, \
    BLTU, \
    BGEU, \
    LB, \
    LH, \
    LW, \
    LBU, \
    LHU, \
    SB, \
    SH, \
    SW, \
    ADDI, \
    SLTI, \
    SLTIU, \
    XORI, \
    ORI, \
    ANDI, \
    SLLI, \
    SRLI, \
    ADD, \
    SUB, \
    SLL, \
    SLT, \
    SLTU, \
    XOR, \
    SRL, \
    SRA, \
    OR, \
    AND, \
    FENCE,

#define TO_STR(...) #__VA_ARGS__

enum class Instructions {
    INSTRUCTIONS
};

struct Assembler {
    using raw_type = std::uint32_t;

    raw_type encode(const std::string &line) const;

private:
    const static inline std::vector<std::string> instr2str = { TO_STR(INSTRUCTIONS) };

    raw_type encode_addi(const std::string &operands) const;
    raw_type encode_slti(const std::string &operands) const;
    raw_type encode_sltiu(const std::string &operands) const;
    raw_type encode_xori(const std::string &operands) const;
    raw_type encode_ori(const std::string &operands) const;
    raw_type encode_andi(const std::string &operands) const;
    // raw_type encode_slli(const std::string &operands) const;
    // raw_type encode_srli(const std::string &operands) const;
    raw_type encode_add(const std::string &operands) const;
    raw_type encode_sub(const std::string &operands) const;
    raw_type encode_sll(const std::string &operands) const;
    raw_type encode_slt(const std::string &operands) const;
    raw_type encode_sltu(const std::string &operands) const;
    raw_type encode_xor(const std::string &operands) const;
    raw_type encode_srl(const std::string &operands) const;
    raw_type encode_sra(const std::string &operands) const;
    raw_type encode_or(const std::string &operands) const;
    raw_type encode_and(const std::string &operands) const;

    // return register index
    std::int32_t get_reg_id(const std::string &reg) const;
    Instructions str2instr(std::string instr_str) const;

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
                        const std::string &operands) const;

    raw_type I_type_aux(const std::int32_t opcode,
                        const std::int32_t funct3,
                        const std::string &operands) const;
};

#undef TO_STR
#undef INSTRUCTIONS

}


#endif

#ifndef INCLUDE_SEM_ANALYZER
#define INCLUDE_SEM_ANALYZER

#include "ast.hpp"
#include "internal-expression.hpp"
#include <list>
#include <optional>

namespace compiler {

enum class Instructions {
    LUI = 0,
    AUIPC,
    JAL,
    JALR,
    BEQ,
    BNE,
    BLT,
    BGE,
    BLTU,
    BGEU,
    LB,
    LH,
    LW,
    LBU,
    LHU,
    SB,
    SH,
    SW,
    ADDI,
    SLTI,
    SLTIU,
    XORI,
    ORI,
    ANDI,
    SLLI,
    SRLI,
    ADD,
    SUB,
    SLL,
    SLT,
    SLTU,
    XOR,
    SRL,
    SRA,
    OR,
    AND,
    FENCE,
    Size,
};

using register_id_type = std::uint8_t;
using imm_value_type = std::int16_t;

struct SymboledValue {
    std::string name;
    std::uint32_t val;
    const std::uint32_t nest;
    const std::uint32_t offset;

    SymboledValue(std::string name_arg, 
                  const std::uint32_t val_arg,
                  const std::uint32_t nest_arg, 
                  const std::uint32_t offset_arg);

    SymboledValue(const std::uint32_t nest_arg, 
                  const std::uint32_t offset_arg);
};

struct Operand {
    enum class Type { Reg, Imm, RefMem, } tag;
    union {
        register_id_type reg_id;
        imm_value_type imm;
        std::pair<register_id_type, imm_value_type> mem;
    } value;
};

struct ThreeAddressCode {
    Instructions instr;
    std::optional<Operand> op1, op2, op3;

    ThreeAddressCode(const Instructions instr);
    ThreeAddressCode(const Instructions instr, const Operand op1);
    ThreeAddressCode(const Instructions instr, const Operand op1, const Operand op2);
    ThreeAddressCode(const Instructions instr, const Operand op1, const Operand op2, const Operand op3);
    ThreeAddressCode(ThreeAddressCode&&) = default;
};

struct SymbolTable {
    constexpr static arg_regs_num = 8;
    using symbols_type = std::list<SymboledValue>;
    using arg_regs_map = std::array<SymboledValue*, arg_regs_num>;

    SymbolTable();
    void set_arg_mapping(const LambdaNode *lambda);
    void restore_arg_mapping();

private:
    arg_regs_map gen_default_map() const;

    symbols_type symbols;
    arg_regs_map regs;
    std::stack<arg_regs_map, std::vector<arg_regs_map>> history;
};

struct SemanticAnalyzer : public ConstNodeVisitor {
    virtual void visit(const EvalNode *node) override;
    virtual void visit(const LambdaNode *node) override;
    virtual void visit(const SymbolNode *node) override;
    virtual void visit(const ConstantNode *node) override;

private:
    using table_ptr = std::unique_ptr<SymbolTable>;

    constexpr static std::size_t callee_saved_regs_cnt = 12;

    SymbolTable table;
    std::uint32_t cur_nest;
    std::vector<ThreeAddressCode> codes;

    void callee_prolog(const LambdaNode *lambda);
    void callee_epilog(const LambdaNode *lambda);

    void append_code(ThreeAddressCode code);
    void append_code(const Instructions instr);
    void append_code(const Instructions instr, const Operand op1);
    void append_code(const Instructions instr, const Operand op1, const Operand op2);
    void append_code(const Instructions instr, const Operand op1, const Operand op2, const Operand op3);

    // dst <- offset(base)
    TreeAddressCode load_word(const register_id_type dst, 
                              const register_id_type base, 
                              const imm_value_type offset) const;

    // offset(base) <- src
    ThreeAddressCode store_word(const register_id_type src, 
                                const register_id_type base, 
                                const imm_value_type offset) const;
};

}

#endif

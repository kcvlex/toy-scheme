#include "sem_analyzer.hpp"

namespace compiler {

namespace {

std::int32_t label = 0;

Operand make_reg(const std::uint8_t reg_id) {
    Operand op;
    op.tag = Operand::Type::Reg;
    op.value.reg_id = reg_id;
    return op;
}

Operand make_imm(const std::uint32_t imm) {
    Operand op;
    op.tag = Operand::Type::Imm;
    op.value.imm = imm;
    return op;
}

Operand make_ref_mem(const std::uint8_t reg_id, const std::uint16_t offset) {
    Operand op;
    op.tag = Operand::Type::RefMem;
    op.value.mem = std::make_pair(reg_id, offset);
    return op;
}

constexpr std::uint8_t ra = 1;
constexpr std::uint8_t sp = 2;
constexpr std::uint8_t bp = 8;
constexpr std::uint8_t rv = 10;

}  // anonymous


/******************** SymboledValue ********************/

SymboledValue::SymboledValue(std::string name_arg,
                             std::uint32_t val_arg,
                             const std::uint32_t nest_arg, 
                             const std::uint32_t offset_arg)
    : name(std::move(name_arg)),
      val(val_arg),
      nest(nest_arg),
      offset(offset_arg)
{
}

SymboledValue::SymboledValue(const std::uint32_t nest_arg,
                             const std::uint32_t offset_arg)
    : SymboledValue("", 0, nest_arg, offset_arg)
{
}


/******************** SymboleTable ********************/

SymbolTable::SymbolTable() {
    history.push(gen_default_map());  // sentinel
}

void SymbolTable::set_arg_mapping(const LambdaNode *lambda) {
    // Arguments mapping
    arg_regs_map rmap = gen_default_map();
    for (std::size_t i = 0; i != lambda->get_args().size(); i++) {
        const auto &arg = lambda->get_args()[i];
        rmap[i] = new SymboledValue(arg->get_symbol(), 0, cur_nest, 0);
    }
    history.push(std::move(regs));
    regs = std::move(rmap);
}

SymbolTable::arg_regs_map SymbolTable::gen_default_map() const {
    arg_regs_map res;
    std::fill(std::begin(res), std::end(res), nullptr);
    return res;
}

void SymbolTable::restore_arg_mapping() {
    regs = history.top();
    history.pop();
}


/******************** SemanticAnalyzer ********************/

// Precondition : 0(sp) == return address
// PostCondition : 0(sp) == empty, 4(bp) == return address
void SemanticAnalyzer::callee_prolog(const LambdaNode *lambda) {
    // Save registers
    append_code(Instructions::ADDI, make_reg(sp), make_imm(4));
    append_code(std::move(store_word(8, sp, 0)));
    append_code(std::move(store_word(9, sp, 4));
    for (register_id_type i = 18; i <= 27; i++) {
        auto cnt = i - 18 + 2;
        append_code(std::move(store_word(i, sp, cnt * 4)));
    }

    append_code(Instructions::ADDI, make_reg(bp), make_reg(sp), make_imm(0));
    append_code(Instructions::ADDI, make_reg(sp), make_imm(4 * callee_saved_regs_cnt));

    table.set_arg_mapping(lambda);
}

// Precondition : 0(sp) == return value
// Postcondition : 0(sp) == return address
void SemanticAnalyzer::callee_epilog(const LambdaNode *lambda) {
    // Return value
    append_code(std::move(load_word(rv, sp, 0)));
   
    // Resotore sp
    append_code(Instructions::ADDI, make_reg(sp), make_reg(bp), make_imm(4));

    // Restore registers
    for (register_id_type i = 27; 18 <= i; i--) {
        auto cnt = i - 18 + 2;
        append_code(std::move(load_word(i, bp, cnt * 4)));
    }
    append_code(std::move(load_word(9, bp, 4)));
    append_code(std::move(load_word(8, bp, 0))); // Base pointer
}

}

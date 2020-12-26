#include "sem_analyzer.hpp"
#include <cassert>
#include <iostream>

namespace compiler {

namespace {

std::uint32_t label = 0;

std::string get_label() {
    return "LL" + std::to_string(label++);
}

}  // anonymous


/******************** SymboledValue ********************/

SymboledValue::SymboledValue(std::string name_arg,
                             const std::uint32_t nest_arg, 
                             const imm_value_type offset_arg)
    : name(std::move(name_arg)),
      nest(nest_arg),
      offset(offset_arg)
{
}

SymboledValue::SymboledValue(const std::uint32_t nest_arg,
                             const imm_value_type offset_arg)
    : SymboledValue("", nest_arg, offset_arg)
{
}


/******************** SymboleTable ********************/

SymbolTable::SymbolTable() {
    history.push(gen_default_map());  // sentinel
}

void SymbolTable::set_arg_mapping(const LambdaNode *lambda, const std::uint32_t cur_nest) {
    // Arguments mapping
    arg_regs_map rmap = gen_default_map();
    for (std::size_t i = 0; i != lambda->get_args().size(); i++) {
        const auto &arg = lambda->get_args()[i];
        rmap[i] = new SymboledValue(arg->get_symbol(), cur_nest, 0);
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

SymboledValue* SymbolTable::find(const std::string &name) const {
    // Search arguments
    for (const auto ptr : regs) {
        if (ptr == nullptr) break;
        if (ptr->name == name) return ptr;
    }

    // Search stack
    // FIXME : impl

    return nullptr;
}

std::optional<register_id_type> SymbolTable::find_arg(const std::string &name) const {
    for (std::size_t i = 0; i != regs.size(); i++) {
        if (regs[i] == nullptr) break;
        if (regs[i]->name == name) return std::make_optional(regs::argument_regs[i]);
    }
    return std::optional<register_id_type>(std::nullopt);
}


/******************** FunctionCode ********************/

FunctionCode::FunctionCode(std::string label_arg, InputCodeStream cs_arg)
    : label(std::move(label_arg)), cs(std::move(cs_arg))
{
}


/******************** SemanticAnalyzer ********************/

SemanticAnalyzer::SemanticAnalyzer() {
    fcodes.emplace_back("", InputCodeStream());  // sentinel
}

std::vector<FunctionCode> SemanticAnalyzer::analyze(const std::vector<ASTNode*> &nodes) {
    for (auto node : nodes) {
        node->accept(*this);
    }
    return fcodes;
}

// Precondition : 0(sp) == return address
// PostCondition : 0(sp) == empty, 4(bp) == return address
InputCodeStream SemanticAnalyzer::callee_prolog(const LambdaNode *lambda) {
    InputCodeStream res;

    // Save registers
    {
        res.append_code(Instructions::ADDI, make_reg(regs::sp), make_imm(4));
        
        for (int i = 0; i < int(regs::callee_saved_regs.size()); i++) {
            const auto reg = regs::callee_saved_regs[i];
            res.append_sw_code(reg, regs::sp, 4 * i);
        }

        res.append_code(Instructions::ADD, make_reg(regs::bp), make_reg(regs::sp), make_reg(0))
           .append_code(Instructions::ADDI, make_reg(regs::sp), make_imm(4 * regs::callee_saved_regs.size()));
    }

    // Arguments
    table.set_arg_mapping(lambda, cur_nest);

    return res;
}

// Precondition : 0(sp) == return value
// Postcondition : 0(sp) == return address
InputCodeStream SemanticAnalyzer::callee_epilog(const LambdaNode *lambda) {
    InputCodeStream res;
    
    res.append_lw_code(regs::rv, regs::sp, 0)  // Return value
       .append_code(Instructions::ADDI, make_reg(regs::sp), make_reg(regs::bp), make_imm(4));  // Resotore sp

    // Restore registers
    {
        for (int i = int(regs::callee_saved_regs.size()) - 1; 0 <= i; i--) {
            const auto reg = regs::callee_saved_regs[i];
            res.append_lw_code(reg, regs::sp, 4 * i);
        }
    
        res.append_lw_code(9, regs::bp, 4)
           .append_lw_code(8, regs::bp, 0);  // base pointer
    }

    // Arguments
    table.restore_arg_mapping();

    return res;
}

void SemanticAnalyzer::visit(const SymbolNode *node) {
    const auto &symbol = node->get_symbol();
    if (symbol == "+") {
        op_stk.push(Operation::ADD);
        return;
    } else if (symbol == "-") {
        op_stk.push(Operation::SUB);
        return;
    }

    op_stk.push(Operation::FUNC);

    const auto reg = table.find_arg(symbol);
    if (reg.has_value()) {
        cur_code.append_code(Instructions::OR, make_reg(regs::rv), make_reg(*reg), make_reg(*reg));
        return;
    }

    const auto value = table.find(symbol);
    if (value) {
        cur_code.append_lw_code(regs::rv, regs::bp, value->offset);
        return;
    }

    assert(false);
}

void SemanticAnalyzer::visit(const ConstantNode *node) {
    cur_code.append_code(Instructions::ADDI, make_reg(regs::rv), make_reg(regs::zero), make_imm(node->get_value()));
}

void SemanticAnalyzer::visit(const LambdaNode *node) {
    code_buf.push_back(std::move(cur_code));
    const auto f_label = get_label();

    cur_code = std::move(callee_prolog(node));
    cur_nest++;
    node->get_body()->accept(*this);
    cur_nest--;
    cur_code.concat_stream(callee_epilog(node));

    fcodes.emplace_back(f_label, std::move(cur_code));
    cur_code = std::move(code_buf.back());
    code_buf.pop_back();
}

void SemanticAnalyzer::visit(const EvalNode *node) {
    const auto op_node = node->get_children()[0];

    SimpleInstructionChecker checker;
    op_node->accept(checker);
    const auto is_simple = checker.get();  // true if ADD or SUB

    // save return address, arguments
    if (!is_simple) {
        cur_code.append_push_code(regs::ra);
        for (register_id_type reg = 10; reg <= 17; reg++) cur_code.append_push_code(reg);
    }

    op_node->accept(*this);
    const auto op = op_stk.top();
    op_stk.pop();

    if (op == Operation::FUNC) {
        assert(!is_simple);
        // rv has the instruction address
        cur_code.append_push_code(regs::rv);

        // eval arguments
        register_id_type r_cnt = 9;
        for (std::size_t i = 1; i < node->get_children().size(); i++, r_cnt++) {
            node->get_children()[i]->accept(*this);
            cur_code.append_push_code(regs::rv);
        }

        // set arguments
        for (register_id_type reg = r_cnt; 10 <= reg; reg--) cur_code.append_pop_code(reg);

        // load function address
        cur_code.append_pop_code(regs::t0)
                .append_code(Instructions::JALR, make_reg(regs::t0), make_reg(regs::ra), make_reg(0));

        // restore arguments, ra
        for (register_id_type reg = 10; reg <= 17; reg++) cur_code.append_pop_code(reg);
        cur_code.append_pop_code(regs::ra);
    } else {
        assert(is_simple);
        
        // Number of the operand must be 2

        // Eval op1
        node->get_children()[1]->accept(*this);
        cur_code.append_push_code(regs::rv);

        // Eval op2
        node->get_children()[2]->accept(*this);
        cur_code.append_push_code(regs::rv);

        const Instructions instr = (op == Operation::ADD ? Instructions::ADD : Instructions::SUB);
        cur_code.append_pop_code(regs::t2)
                .append_pop_code(regs::t1)
                .append_code(instr, make_reg(regs::rv), make_reg(regs::t1), make_reg(regs::t2));
    }
}


/******************** SimpleInstructionChecker ********************/

void SimpleInstructionChecker::visit(const EvalNode *node) { res = false; }
void SimpleInstructionChecker::visit(const LambdaNode *node) { res = false; }
void SimpleInstructionChecker::visit(const SymbolNode *node) {
    if (node->get_symbol() == "+" || node->get_symbol() == "-") res = true;
    else res = false;
}
void SimpleInstructionChecker::visit(const ConstantNode *node) { assert(false); }
bool SimpleInstructionChecker::get() const noexcept { return res; }

}

#include "sem_analyzer.hpp"
#include <cassert>
#include <iostream>

namespace compiler {

namespace {

std::uint32_t label = 0;

std::string get_label() {
    return "LL" + std::to_string(label++);
}

constexpr Reg bp_reg = Reg::s0;
constexpr Reg rv_reg = Reg::a0;

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

std::optional<Reg> SymbolTable::find_arg(const std::string &name) const {
    for (std::size_t i = 0; i != regs.size(); i++) {
        if (regs[i] == nullptr) break;
        if (regs[i]->name == name) return std::make_optional(nth_arg_reg(i));
    }
    return std::optional<Reg>(std::nullopt);
}


/******************** FunctionCode ********************/

FunctionCode::FunctionCode(std::string label_arg, OutputCodeStream cs_arg)
    : label(std::move(label_arg)), cs(std::move(cs_arg))
{
}


/******************** SemanticAnalyzer ********************/

SemanticAnalyzer::SemanticAnalyzer() : cur_nest(0) {
    fcodes.emplace_back("", OutputCodeStream());  // sentinel
}

std::vector<FunctionCode> SemanticAnalyzer::analyze(const std::vector<ASTNode*> &nodes) {
    for (auto node : nodes) {
        node->accept(*this);
    }
    return fcodes;
}

// Precondition : 0(sp) == return address
// PostCondition : 0(sp) == empty, 4(bp) == return address
OutputCodeStream SemanticAnalyzer::callee_prolog(const LambdaNode *lambda) {
    OutputCodeStream res;

    // Save registers
    {
        res.append_code(Instructions::ADDI,
                        reg2operand(Reg::sp),
                        reg2operand(Reg::sp), 
                        imm2operand(4));

        for (std::size_t i = 0; i < arg_reg_num; i++) {
            const auto reg = nth_arg_reg(i);
            res.append_sw_code(reg, Reg::sp, 4 * i);
        }

        res.append_assign_code(bp_reg, Reg::sp)
           .append_code(Instructions::ADDI, 
                        reg2operand(Reg::sp), 
                        reg2operand(Reg::sp),
                        imm2operand(4 * callee_saved_reg_num));
    }

    // Arguments
    table.set_arg_mapping(lambda, cur_nest);

    return res;
}

// Precondition : 0(sp) == return value
// Postcondition : 0(sp) == return address
OutputCodeStream SemanticAnalyzer::callee_epilog(const LambdaNode *lambda) {
    OutputCodeStream res;

    res.append_lw_code(rv_reg, Reg::sp, 0)  // return value
       .append_code(Instructions::ADDI,
                    reg2operand(Reg::sp),
                    reg2operand(bp_reg),
                    imm2operand(4));     // restore sp

    // Restore registers
    {
        for (std::size_t i = 0; i < callee_saved_reg_num; i++) {
            const auto reg = nth_callee_saved_reg(callee_saved_reg_num - (i + 1));
            res.append_lw_code(reg, Reg::sp, 4 * i);
        }
   
        res.append_lw_code(Reg::s1, bp_reg, 4)
           .append_lw_code(bp_reg, bp_reg, 0); // base_ptr
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
        cur_code.append_assign_code(rv_reg, *reg);
        return;
    }

    const auto value = table.find(symbol);
    if (value) {
        cur_code.append_lw_code(rv_reg, bp_reg, value->offset);
        return;
    }

    assert(false);
}

void SemanticAnalyzer::visit(const ConstantNode *node) {
    cur_code.append_code(Instructions::ADDI,
                         reg2operand(rv_reg),
                         reg2operand(Reg::zero),
                         imm2operand(node->get_value()));
}

void SemanticAnalyzer::visit(const LambdaNode *node) {
    code_buf.push_back(std::move(cur_code));
    cur_code.clear();
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
    const auto &children = node->get_children();
    const auto op_node = children[0];

    SimpleInstructionChecker checker;
    op_node->accept(checker);
    const auto is_simple = checker.get();  // true if ADD or SUB

    // save return address, arguments
    if (!is_simple) {
        cur_code.append_push_code(Reg::ra);
        for (std::size_t i = 0; i < arg_reg_num; i++) cur_code.append_push_code(nth_arg_reg(i));
    }

    op_node->accept(*this);
    const auto op = op_stk.top();
    op_stk.pop();

    if (op == Operation::FUNC) {
        assert(!is_simple);
        // rv has the instruction address
        cur_code.append_push_code(rv_reg);

        // eval arguments
        std::size_t r_cnt = 0;
        for (std::size_t i = 1; i < children.size(); i++) {
            const auto ch = children[i];
            ch->accept(*this);
            cur_code.append_push_code(rv_reg);
            r_cnt++;
        }

        // set arguments
        for (std::size_t i = 0; i < r_cnt; i++) {
            const auto reg = nth_arg_reg(r_cnt - (i + 1));
            cur_code.append_push_code(reg);
        }

        // load function address
        cur_code.append_pop_code(Reg::t0)
                .append_code(Instructions::JALR, 
                             reg2operand(Reg::t0), 
                             reg2operand(Reg::ra), 
                             reg2operand(Reg::zero));

        // restore arguments, ra
        for (std::size_t i = 0; i < arg_reg_num; i++) {
            const auto reg = nth_arg_reg(arg_reg_num - (i + 1));
            cur_code.append_pop_code(reg);
        }
        cur_code.append_pop_code(Reg::ra);
    } else {
        assert(is_simple);
        
        // Number of the operand must be 2

        // Eval op1
        children[1]->accept(*this);
        cur_code.append_push_code(rv_reg);

        // Eval op2
        children[2]->accept(*this);
        cur_code.append_push_code(rv_reg);

        const Instructions instr = (op == Operation::ADD ? Instructions::ADD : Instructions::SUB);
        cur_code.append_pop_code(Reg::t2)
                .append_pop_code(Reg::t1)
                .append_code(instr,
                             reg2operand(rv_reg),
                             reg2operand(Reg::t1),
                             reg2operand(Reg::t2));
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

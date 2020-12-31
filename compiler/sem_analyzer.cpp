#include "sem_analyzer.hpp"
#include <cassert>
#include <iostream>
#include <limits>

namespace compiler {

namespace {

std::uint32_t label = 0;

std::uint32_t get_label() {
    return label++;
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

FunctionCode::FunctionCode(std::uint32_t label_arg, OutputCodeStream ocs_arg)
    : label(label_arg), cs(std::move(ocs_arg))
{
}


/******************** SemanticAnalyzer ********************/

SemanticAnalyzer::SemanticAnalyzer() : cur_nest(0) {
    fcodes.emplace_back(-1, OutputCodeStream());  // sentinel
}

std::vector<FunctionCode> SemanticAnalyzer::analyze(const std::vector<ASTNode*> &nodes) {
    for (auto node : nodes) {
        node->accept(*this);
        if (cur_code.entire_size()) fcodes.emplace_back(get_label(), std::move(cur_code));
        cur_code.clear();
    }

    // call main
    OutputCodeStream call_main_os;
    call_main_os.append_code(Instructions::JAL, 
                             reg2operand(Reg::zero),
                             label2operand(fcodes.back().label));
    FunctionCode call_main(-1, call_main_os);
    fcodes[0] = call_main;
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
#if 0
        for (std::size_t i = 0; i < callee_saved_reg_num; i++) {
            const auto reg = nth_callee_saved_reg(i);
            res.append_sw_code(reg, Reg::sp, 4 * i);
        }

        res.append_assign_code(bp_reg, Reg::sp)
           .append_code(Instructions::ADDI, 
                        reg2operand(Reg::sp), 
                        reg2operand(Reg::sp),
                        imm2operand(4 * callee_saved_reg_num));
#endif
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
#if 0
        for (std::size_t i = 0; i < callee_saved_reg_num; i++) {
            const auto reg = nth_callee_saved_reg(callee_saved_reg_num - (i + 1));
            res.append_lw_code(reg, Reg::sp, 4 * i);
        }
#endif
   
        res.append_lw_code(Reg::s1, bp_reg, 4)
           .append_lw_code(bp_reg, bp_reg, 0); // base_ptr
    }

    // Arguments
    table.restore_arg_mapping();

    return res;
}

void SemanticAnalyzer::visit(const SymbolNode *node) {
    const auto &symbol = node->get_symbol();
    if (symbol == "+" || symbol == "-") return;

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
    const auto res = checker.res.value();

    op_node->accept(*this);
        
    cur_code.append_push_code(rv_reg);  // save a0

    if (auto p = std::get_if<BuiltinOperation>(&res)) {
        // Number of the operand must be 2

        // Eval op1
        children[1]->accept(*this);
        cur_code.append_push_code(rv_reg);

        // Eval op2
        children[2]->accept(*this);
        cur_code.append_push_code(rv_reg);

        const Instructions instr = (*p == BuiltinOperation::ADD ? Instructions::ADD : Instructions::SUB);
        cur_code.append_pop_code(Reg::t2)
                .append_pop_code(Reg::t1)
                .append_pop_code(Reg::a0)  // restore a0
                .append_code(instr,
                             reg2operand(rv_reg),
                             reg2operand(Reg::t1),
                             reg2operand(Reg::t2));
    } else {
        cur_code.append_push_code(Reg::ra);
#if 0
        for (std::size_t i = 0; i < arg_reg_num; i++) cur_code.append_push_code(nth_arg_reg(i));

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
#else
        for (std::size_t i = 1; i != children.size(); i++) {
            const std::size_t idx = children.size() - i;
            children[idx]->accept(*this);
            cur_code.append_assign_code(nth_arg_reg(idx), rv_reg);
        }
        cur_code.append_pop_code(Reg::t0);  // restore a0
#endif

        // load function address
        if (auto p = std::get_if<lambda_function_type>(&res)) {
            cur_code.append_pop_code(Reg::zero)  // Unnecessary value
                    .append_code(Instructions::JAL,
                                 reg2operand(Reg::t0),  // FIXME : Reg::zero ??
                                 label2operand(*p));
        } else if (auto p = std::get_if<symbol_type>(&res)) {
            const auto reg = table.find_arg((*p)->get_symbol()).value();
            cur_code.append_pop_code(Reg::zero)  // Unnecessary value
                    .append_code(Instructions::JALR,
                                 reg2operand(Reg::t0),  // FIXME : Reg::zero ??
                                 reg2operand(reg),
                                 imm2operand(0));

        } else if (auto p = std::get_if<eval_type>(&res)) {
            cur_code.append_pop_code(Reg::t1)  // jump address
                    .append_code(Instructions::JALR,
                                 reg2operand(Reg::t0),  // FIXME : Reg::zero ??
                                 reg2operand(Reg::t1),
                                 imm2operand(0));
        }

#if 0
        // restore arguments, ra
        for (std::size_t i = 0; i < arg_reg_num; i++) {
            const auto reg = nth_arg_reg(arg_reg_num - (i + 1));
            cur_code.append_pop_code(reg);
        }
#endif
        cur_code.append_pop_code(Reg::ra);
    }
}


/******************** SimpleInstructionChecker ********************/

namespace {

operation_type make_builtin_operation(const BuiltinOperation op) {
    return operation_type(std::in_place_type<BuiltinOperation>, op);
}

operation_type make_lambda_function(const lambda_function_type lf) {
    return operation_type(std::in_place_type<lambda_function_type>, lf);
}

operation_type make_symbol_type(const symbol_type node) {
    return operation_type(std::in_place_type<symbol_type>, node);
}

operation_type make_eval() {
    return operation_type(std::in_place_type<eval_type>);
}

}  // anonymous

void SimpleInstructionChecker::visit(const EvalNode *node) { 
    res = std::optional(make_eval());
}
void SimpleInstructionChecker::visit(const LambdaNode *node) {
    res = std::optional(make_lambda_function(label));
}
void SimpleInstructionChecker::visit(const SymbolNode *node) {
    if (node->get_symbol() == "+") {
        res = std::optional(make_builtin_operation(BuiltinOperation::ADD));
    } else if (node->get_symbol() == "-") {
        res = std::optional(make_builtin_operation(BuiltinOperation::SUB));
    } else {
        res = std::optional(make_symbol_type(node));
    }
}
void SimpleInstructionChecker::visit(const ConstantNode *node) { 
}

}

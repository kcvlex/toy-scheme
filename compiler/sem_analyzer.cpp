#include "sem_analyzer.hpp"
#include <cassert>
#include <iostream>
#include <limits>
#include <utility>
#include <algorithm>

namespace compiler {

namespace {

std::uint32_t label = 0;

std::uint32_t get_label() {
    return label++;
}

}  // anonymous


/******************** FunctionCode ********************/

FunctionCode::FunctionCode(std::uint32_t label_arg, OutputCodeStream ocs_arg)
    : label(label_arg), cs(std::move(ocs_arg))
{
}


/******************** SemanticAnalyzer ********************/

SemanticAnalyzer::SemanticAnalyzer() : cur_nest(0) {
    // sentinel
    fcodes.emplace_back(-1, OutputCodeStream());
    sra_stk.emplace(0);
}

std::vector<FunctionCode> SemanticAnalyzer::analyze(const SequenceNode* const node) {
    node->accept(*this);

    // FIXME : address 0 means `exit`
    OutputCodeStream main_ocs;
    main_ocs.append_nop_code()
            .append_nop_code()
            .append_nop_code()
            .append_nop_code()
            .concat(std::move(cur_code))
            .append_code(Instructions::JALR,
                         reg2operand(Reg::zero),
                         reg2operand(Reg::zero),
                         imm2operand(0));
    FunctionCode call_main(-1, main_ocs);
    fcodes[0] = call_main;
    return fcodes;
}

// Precondition : Reg::ra == return address
// PostCondition : 0(sp) == empty
OutputCodeStream SemanticAnalyzer::callee_prolog() {
    // save regs
    auto res = std::move(OutputCodeStream::save_callee_saved_regs(sra_stk.top()));
    SimpleRegisterAllocator sra(sra_stk.top().arg_num());
    sra_stk.push(sra);

    return res;
}

// Precondition : rv_reg == return value, Reg::ra == return address
OutputCodeStream SemanticAnalyzer::callee_epilog() {
    // restore regs
    sra_stk.pop();
    auto res = std::move(OutputCodeStream::restore_callee_saved_regs(sra_stk.top()));

    // return
    res.append_code(Instructions::JALR,
                    reg2operand(Reg::zero),
                    reg2operand(Reg::ra),
                    imm2operand(0));
    return res;
}

void SemanticAnalyzer::visit(const SymbolNode* const node) {
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

void SemanticAnalyzer::visit(const ConstantNode* const node) {
    cur_code.append_code(Instructions::ADDI,
                         reg2operand(rv_reg),
                         reg2operand(Reg::zero),
                         imm2operand(node->get_value()));
}

void SemanticAnalyzer::visit(const LambdaNode* const node) {
    // set args map
    table.set_arg_map(node, cur_nest);

    code_buf.push_back(std::move(cur_code));
    cur_code.clear();
    const auto f_label = get_label();

    // body
    cur_nest++;
    node->get_body()->accept(*this);
    cur_nest--;

    // prolog and epilog
    OutputCodeStream res;
    res.concat(std::move(callee_prolog()))
       .concat(std::move(cur_code))
       .concat(std::move(callee_epilog()));

    // restore args map
    table.restore_arg_map();

    fcodes.emplace_back(f_label, std::move(res));
    cur_code = std::move(code_buf.back());
    code_buf.pop_back();
}

void SemanticAnalyzer::visit(const EvalNode* const node) {
    const auto &children = node->get_children();
    const auto op_node = children[0];

    // save regs
    {
        auto tmp = std::move(OutputCodeStream::save_caller_saved_regs(sra_stk.top()));
        cur_code.concat(std::move(tmp));
    }

    SimpleInstructionChecker checker;
    op_node->accept(checker);
    const auto res = checker.res.value();

    if (auto p = std::get_if<BuiltinOperation>(&res)) {
        // Number of the operand must be 2

        // Eval op1
        children[1]->accept(*this);
        const auto op1 = sra_stk.top().get_callee_saved_reg();;
        cur_code.append_assign_code(op1, rv_reg);

        // Eval op2
        children[2]->accept(*this);

        const Instructions instr = (*p == BuiltinOperation::ADD ? Instructions::ADD : Instructions::SUB);
        cur_code.append_code(instr,
                             reg2operand(rv_reg),
                             reg2operand(op1),
                             reg2operand(rv_reg));
    } else {
        op_node->accept(*this);

        std::array<Reg, 32> reg_arr;
        std::size_t reg_arr_idx = 0;

        auto get_reg = [&] {
            const auto reg = sra_stk.top().get_callee_saved_reg();
            return reg_arr[reg_arr_idx++] = reg;
        };
        
        // save function address
        const auto f_addr_reg = get_reg();
        cur_code.append_assign_code(f_addr_reg, rv_reg);
        
        // eval arguments
        for (std::size_t i = 1; i != children.size(); i++) {
            children[i]->accept(*this);
            const auto dst = get_reg();
            cur_code.append_assign_code(dst, rv_reg);
        }

        // set arguments
        for (std::size_t i = 1; i != children.size(); i++) {
            cur_code.append_assign_code(nth_arg_reg(i - 1), reg_arr[i]);
        }

        // call function
        if (auto p = std::get_if<lambda_function_type>(&res)) {
            cur_code.append_code(Instructions::JAL,
                                 reg2operand(Reg::ra),
                                 label2operand(*p));

        } else if (auto p = std::get_if<symbol_type>(&res)) {
            const auto reg = table.find_arg((*p)->get_symbol()).value();
            cur_code.append_code(Instructions::JALR,
                                 reg2operand(Reg::ra),
                                 reg2operand(reg),
                                 imm2operand(0));

        } else if (auto p = std::get_if<eval_type>(&res)) {
            cur_code.append_code(Instructions::JALR,
                                 reg2operand(Reg::ra),
                                 reg2operand(f_addr_reg),
                                 imm2operand(0));
        }
    }

    // restore regs
    {
        auto tmp = std::move(OutputCodeStream::restore_caller_saved_regs(sra_stk.top()));
        cur_code.concat(std::move(tmp));
    }
}


void SemanticAnalyzer::visit(const SequenceNode* const node) {
    cur_code.append_push_code(Reg::ra);
    for (auto expr : node->get_seq()) expr->accept(*this);
    cur_code.append_pop_code(Reg::ra)
            .append_code(Instructions::JALR,
                         reg2operand(Reg::zero),
                         reg2operand(Reg::ra),
                         imm2operand(0));
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

void SimpleInstructionChecker::visit(const EvalNode* const node) { 
    res = std::optional(make_eval());
}
void SimpleInstructionChecker::visit(const LambdaNode* const node) {
    res = std::optional(make_lambda_function(label));
}
void SimpleInstructionChecker::visit(const SymbolNode* const node) {
    if (node->get_symbol() == "+") {
        res = std::optional(make_builtin_operation(BuiltinOperation::ADD));
    } else if (node->get_symbol() == "-") {
        res = std::optional(make_builtin_operation(BuiltinOperation::SUB));
    } else {
        res = std::optional(make_symbol_type(node));
    }
}
void SimpleInstructionChecker::visit(const ConstantNode* const node) { 
}
void SimpleInstructionChecker::visit(const SequenceNode* const node) {
    res = std::optional(make_eval());
}

}

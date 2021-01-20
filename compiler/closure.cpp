#include "closure.hpp"
#include <iostream>

namespace compiler {

void CollectExternRefs::visit(LambdaCPS* const cps) {
    // into another scope
}

void CollectExternRefs::visit(PrimitiveCPS* const cps) {
}

void CollectExternRefs::visit(ApplyCPS* const cps) {
    cps->get_proc()->accept(*this);
    for (std::size_t i = 0; i != cps->get_arg_num(); i++) {
        cps->get_arg(i)->accept(*this);
    }
}

void CollectExternRefs::visit(BindCPS* const cps) {
    // maybe unused
}

void CollectExternRefs::visit(VarCPS* const cps) {
    decltype(auto) name = cps->get_var();
    if (is_inner_var(name)) return;
    outer.insert(name);
    cps->clsr_flag = true;
}

void CollectExternRefs::visit(ConstantCPS* const cps) {
}

bool CollectExternRefs::is_inner_var(const std::string &name) const noexcept {
    auto ite = inner.find(name);
    return ite != inner.end();
}

void set_closure(LambdaCPS* const lambda) {
    CollectExternRefs cer;

    for (std::size_t i = 0; i != lambda->get_arg_num(); i++) {
        cer.inner.insert(lambda->get_arg(i));
    }

    for (std::size_t i = 0; i != lambda->get_bind_num(); i++) {
        const auto bind = lambda->get_bind(i);
        cer.inner.insert(bind->get_name());
    }

    lambda->get_body()->accept(cer);
    if (!cer.outer.empty()) lambda->clsr = new ClosureTable(cer.outer);
}
    
void ClosureTranslator::visit(LambdaCPS* const cps) {
    set_closure(cps);

    const auto store = this->clsr;
    this->clsr = cps->clsr;

    for (std::size_t i = 0; i != cps->get_bind_num(); i++) {
        const auto bind = cps->get_bind(i);
        bind->accept(*this);
    }

    cps->get_body()->accept(*this);

    this->clsr = store;
}

void ClosureTranslator::visit(ApplyCPS* const cps) {
    cps->get_proc()->accept(*this);
    for (std::size_t i = 0; i != cps->get_arg_num(); i++) {
        cps->get_arg(i)->accept(*this);
    }
}

void ClosureTranslator::visit(BindCPS* const cps) {
    cps->get_value()->accept(*this);
}

void ClosureTranslator::visit(VarCPS* const cps) {
    if (!cps->clsr_flag) return;
    cps->set_clsr(clsr);
}

void ClosureTranslator::visit(PrimitiveCPS* const cps) {
}

void ClosureTranslator::visit(ConstantCPS* const cps) {
}

void closure_translation(CPSNode* const root) {
    ClosureTranslator ct;
    root->accept(ct);
}

}

#include "closure.hpp"
#include <iostream>

namespace compiler {

void CollectExternRefs::visit(LambdaCPS* const cps) {
    std::set<std::string> added;

    auto add_local_var = [&](const std::string &s) {
        const auto sz0 = inner.size();
        inner.insert(s);
        const auto sz1 = inner.size();
        if (sz0 != sz1) added.insert(s);
    };

    for (std::size_t i = 0; i != cps->get_arg_num(); i++) {
        add_local_var(cps->get_arg(i));
    }

    for (std::size_t i = 0; i != cps->get_bind_num(); i++) {
        const auto bind = cps->get_bind(i);
        add_local_var(bind->get_name());
        bind->get_value()->accept(*this);
    }

    cps->get_body()->accept(*this);

    for (const auto &e : added) inner.erase(e);
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
    cps->lex_scope_flag = true;
}

void CollectExternRefs::visit(ConstantCPS* const cps) {
}

bool CollectExternRefs::is_inner_var(const std::string &name) const noexcept {
    auto ite = inner.find(name);
    return ite != inner.end();
}

void set_closure(LambdaCPS* const lambda) {
    CollectExternRefs cer;
    lambda->accept(cer);
    if (!cer.outer.empty()) lambda->lex_scope = new LexicalScope(cer.outer);
}
    
void ClosureTranslator::visit(LambdaCPS* const cps) {
    set_closure(cps);

    const auto store = this->lex_scope;
    this->lex_scope = cps->lex_scope;

    for (std::size_t i = 0; i != cps->get_bind_num(); i++) {
        const auto bind = cps->get_bind(i);
        bind->accept(*this);
    }

    cps->get_body()->accept(*this);

    this->lex_scope = store;
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
    if (!cps->lex_scope_flag) return;
    cps->set_lex_scope(lex_scope);
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

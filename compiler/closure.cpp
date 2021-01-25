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
    }

    for (std::size_t i = 0; i != cps->get_bind_num(); i++) {
        const auto bind = cps->get_bind(i);
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
}

void CollectExternRefs::visit(ConstantCPS* const cps) {
}

bool CollectExternRefs::is_inner_var(const std::string &name) const noexcept {
    auto ite = inner.find(name);
    return ite != inner.end();
}

void set_lex_scope(LambdaCPS* const lambda) {
    CollectExternRefs cer;
    lambda->accept(cer);

    std::vector<std::string> args, locals;
    for (std::size_t i = 0; i != lambda->get_arg_num(); i++) {
        args.push_back(lambda->get_arg(i));
    }
    for (std::size_t i = 0; i != lambda->get_bind_num(); i++) {
        locals.push_back(lambda->get_bind(i)->get_name());
    }

    auto ext_refs = std::make_shared<ExternRefs>(cer.outer);
    lambda->lex_scope = std::make_shared<LexicalScope>(std::move(args),
                                                       std::move(locals),
                                                       ext_refs);
}
   
void ClosureTranslator::visit(LambdaCPS* const cps) {
    set_lex_scope(cps);

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
    cps->ref = this->lex_scope->get_ref(cps->get_var());
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

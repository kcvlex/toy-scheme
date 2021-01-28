#include "closure.hpp"
#include <iostream>
#include <unordered_map>
#include <cassert>

namespace compiler {

namespace {

std::unordered_map<LambdaCPS*, internal::ClosureRecordDistributor> crd_map;

void add_child(LambdaCPS* const par,
               LambdaCPS* const child)
{
    auto ite = crd_map.find(par);
    assert(ite != std::end(crd_map));
    ite->second.add_child(child);
}

void distribute_clsr_record(LambdaCPS* const lambda) {
    auto ite = crd_map.find(lambda);
    assert(ite != std::end(crd_map));
    ite->second.build_clsr_record_factory();
    ite->second.distribute();
}

}  // anonymouse

namespace internal {


/******************** ClosureRecordDistributor ********************/

ClosureRecordDistributor::ClosureRecordDistributor(LambdaCPS* const lambda_arg)
    : lambda(lambda_arg),
      crf(nullptr),
      children()
{
}

void ClosureRecordDistributor::add_child(LambdaCPS* const child) {
    children.push_back(child);
}

void ClosureRecordDistributor::build_clsr_record_factory() {
    ClosureRecordFactoryBuilder builder;
    for (const auto child : children) builder.append(child->get_raw_ext_refs());
    auto ptr = std::make_unique<ClosureRecordFactory>(std::move(builder.build()));
    crf.swap(ptr);
}

void ClosureRecordDistributor::distribute() {
    for (const auto child : children) {
        child->set_clsr_record(crf->produce());
    }
}


/******************** ExternRefsCollector ********************/

ExternRefsCollector::ExternRefsCollector(LambdaCPS* const lambda)
    : inner(),
      outer(),
      root(lambda),
      lambda_cnt(-1)
{
    ClosureRecordDistributor crd(root);
    crd_map.emplace(root, std::move(crd));
}

void ExternRefsCollector::visit(LambdaCPS* const cps) {
    lambda_cnt++;

    if (lambda_cnt == 1) add_child(root, cps);

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

    lambda_cnt--;
}

void ExternRefsCollector::visit(PrimitiveCPS* const cps) {
}

void ExternRefsCollector::visit(ApplyCPS* const cps) {
    cps->get_proc()->accept(*this);
    for (std::size_t i = 0; i != cps->get_arg_num(); i++) {
        cps->get_arg(i)->accept(*this);
    }
}

void ExternRefsCollector::visit(BindCPS* const cps) {
    // maybe unused
}

void ExternRefsCollector::visit(VarCPS* const cps) {
    decltype(auto) name = cps->get_var();
    if (is_inner_var(name)) return;
    outer.insert(name);
}

void ExternRefsCollector::visit(ConstantCPS* const cps) {
}

bool ExternRefsCollector::is_inner_var(const std::string &name) const noexcept {
    auto ite = inner.find(name);
    return ite != inner.end();
}


/******************** ClosureTranslator ********************/

void ClosureTranslator::visit(LambdaCPS* const cps) {
    set_ext_refs(cps);
    distribute_clsr_record(cps);

    const auto store = this->lex_scope;
    this->lex_scope = cps->get_lex_scope();

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

}  // internal


void set_ext_refs(LambdaCPS* const lambda) {
    internal::ExternRefsCollector erc(lambda);
    lambda->accept(erc);
    auto &v = lambda->ext_refs;
    v.insert(std::end(v), std::begin(erc.outer), std::end(erc.outer));
}

void closure_translation(CPSNode* const root) {
    internal::ClosureTranslator ct;
    root->accept(ct);
}


}  // compiler

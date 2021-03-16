#include "closure.hpp"
#include <iostream>
#include <unordered_map>
#include <cassert>
#include "util.hpp"

namespace compiler {

namespace internal {

void analyze_dependency(CPSNode* const root) {
    DependencyAnalyzer da;
    root->accept(da);
}


/******************** LambdaTree ********************/

void LambdaTree::add_edge(node_ptr_type const parent,
                          node_ptr_type const child)
{
    children[parent].insert(child);
}


/******************** ClosureRecordSetter ********************/

ClosureRecordSetter::ClosureRecordSetter(LambdaCPS* const lambda_arg)
    : lambda(lambda_arg),
      crf(nullptr)
{
}

void ClosureRecordSetter::set(LambdaTree &tree) {
    build_crf();
    set_passing_record();
    tree.for_each_child(lambda, [&](LambdaCPS* const child) { pass_record(child); });
}

void ClosureRecordSetter::set_passing_record() {
    auto copy = *crf->get_raw_record();
    lambda->to_pass = std::make_unique<refs_seq_type>(std::move(copy));
}

void ClosureRecordSetter::pass_record(LambdaCPS* const child) {
    child->set_clsr_record(crf->produce());
}

void ClosureRecordSetter::build_crf() {
    ClosureRecordFactoryBuilder builder;

    builder.append(lambda->get_raw_ext_refs());

    lambda->for_each_args(
        [&](const std::string &arg, std::size_t) {
            builder.append(arg);
        });

    lambda->for_each_binds(
        [&](const BindCPS* const bind, std::size_t) {
            builder.append(bind->get_name());
        });
    
    auto ptr = std::make_unique<ClosureRecordFactory>(std::move(builder.build()));
    crf.swap(ptr);
}


/******************** BaseVisitor ********************/

void BaseVisitor::visit(LambdaCPS* const cps) {
    cps->for_each_binds(
        [&](BindCPS* const bind, std::size_t) {
            bind->accept(*this);
        });

    cps->get_body()->accept(*this);
}

void BaseVisitor::visit(PrimitiveCPS* const cps) {
}

void BaseVisitor::visit(ApplyCPS* const cps) {
    cps->get_proc()->accept(*this);
    cps->for_each_args(
        [&](CPSNode::node_ptr const node, std::size_t) {
            node->accept(*this);
        });
}

void BaseVisitor::visit(BindCPS* const cps) {
    cps->get_value()->accept(*this);
}

void BaseVisitor::visit(VarCPS* const cps) {
}

void BaseVisitor::visit(ConstantCPS* const cps) {
}


/******************** ExternRefsCollector ********************/

ExternRefsCollector::ExternRefsCollector(LambdaCPS* const lambda)
    : inner(),
      outer(),
      root(lambda)
{
    root->accept(*this);
}

void ExternRefsCollector::visit(LambdaCPS* const cps) {
    std::set<std::string> added;
    auto add_local_var = [&](const std::string &s) {
        const auto sz0 = inner.size();
        inner.insert(s);
        const auto sz1 = inner.size();
        if (sz0 != sz1) added.insert(s);
    };

    cps->for_each_args(
        [&](const std::string &arg, std::size_t) {
            add_local_var(arg);
        });

    cps->for_each_binds(
        [&](BindCPS* const bind, std::size_t) {
            add_local_var(bind->get_name());
        });

    BaseVisitor::visit(cps);

    for (const auto &e : added) inner.erase(e);
}

void ExternRefsCollector::visit(VarCPS* const cps) {
    decltype(auto) name = cps->get_var();
    if (!is_inner_var(name)) outer.insert(name);
}

bool ExternRefsCollector::is_inner_var(const std::string &name) const noexcept {
    auto ite = inner.find(name);
    return ite != inner.end();
}

const std::set<std::string>& ExternRefsCollector::get_ext_refs() const noexcept {
    return outer;
}


/******************** DependencyAnalyzer ********************/

void DependencyAnalyzer::visit(LambdaCPS* const cps) {
    /* Build Tree */
    {
        if (par_lambda) tree.add_edge(par_lambda, cps);
        const auto store = par_lambda;
        par_lambda = cps;

        BaseVisitor::visit(cps);
        
        par_lambda = store;
    }

    /* Set Record */
    {
        ExternRefsCollector erc(cps);

        auto &v = cps->ext_refs;
        const auto &set = erc.get_ext_refs();
        v.insert(std::end(v), std::cbegin(set), std::cend(set));

        ClosureRecordSetter setter(cps);
        setter.set(tree);
    }
}


/******************** ClosureTranslator ********************/

void ClosureTranslator::visit(LambdaCPS* const cps) {
    const auto store = this->lex_scope;
    this->lex_scope = cps->get_lex_scope();
    assert(this->lex_scope);

    BaseVisitor::visit(cps);

    this->lex_scope = store;
}

void ClosureTranslator::visit(VarCPS* const cps) {
    cps->ref = this->lex_scope->get_ref(cps->get_var());
}

}  // internal

void closure_translation(CPSNode* const root) {
    internal::analyze_dependency(root);
    internal::ClosureTranslator ct;
    root->accept(ct);
}


}  // compiler

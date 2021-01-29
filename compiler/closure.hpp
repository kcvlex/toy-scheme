#ifndef INCLUDE_COMPILER_CLOSURE
#define INCLUDE_COMPILER_CLOSURE

#include <memory>
#include <unordered_set>
#include <unordered_map>
#include "cps.hpp"
#include "lexical_scope.hpp"

namespace compiler {

void closure_translation(CPSNode* const root);

namespace internal {

void analyze_dependency(CPSNode* const root);

struct LambdaTree {
    using node_ptr_type = LambdaCPS*;
    using children_type = std::unordered_set<node_ptr_type>;

    void add_edge(node_ptr_type const parent,
                  node_ptr_type const child);

    template <typename F>
    void for_each_child(node_ptr_type node, F f) {
        const auto ite = children.find(node);
        if (ite == std::end(children)) return;
        auto &set = ite->second;
        for (node_ptr_type const child : set) f(child);
    }

private:
    std::unordered_map<node_ptr_type, children_type> children;
};

struct ClosureRecordSetter {
    ClosureRecordSetter() = delete;
    ClosureRecordSetter(LambdaCPS* const lambda_arg);

    void set(LambdaTree &tree);

private:
    LambdaCPS *lambda;
    std::unique_ptr<ClosureRecordFactory> crf;

    void set_passing_record();
    void pass_record(LambdaCPS* const child);
    void build_crf();
};

struct BaseVisitor : public ModifyCPSVisitor {
    virtual void visit(LambdaCPS* const cps) override;
    virtual void visit(PrimitiveCPS* const cps) override;
    virtual void visit(ApplyCPS* const cps) override;
    virtual void visit(BindCPS* const cps) override;
    virtual void visit(VarCPS* const cps) override;
    virtual void visit(ConstantCPS* const cps) override;
};

struct ExternRefsCollector : public BaseVisitor {
    ExternRefsCollector(LambdaCPS* const lambda);

    virtual void visit(LambdaCPS* const cps) override;
    virtual void visit(VarCPS* const cps) override;

    const std::set<std::string>& get_ext_refs() const noexcept;

private:
    std::set<std::string> inner, outer;
    LambdaCPS *root;

    bool is_inner_var(const std::string &name) const noexcept;
};

struct DependencyAnalyzer : public BaseVisitor {
    virtual void visit(LambdaCPS* const cps) override;

private:
    LambdaCPS *par_lambda = nullptr;
    LambdaTree tree;
};

struct ClosureTranslator : public BaseVisitor {
    virtual void visit(LambdaCPS* const cps) override;
    virtual void visit(VarCPS* const cps) override;

private:
    const LexicalScope *lex_scope = nullptr;
};

}

}

#endif

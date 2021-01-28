#ifndef INCLUDE_COMPILER_CLOSURE
#define INCLUDE_COMPILER_CLOSURE

#include <memory>
#include "cps.hpp"
#include "lexical_scope.hpp"

namespace compiler {

void set_ext_refs(LambdaCPS* const lambda);
void closure_translation(CPSNode* const root);

namespace internal {

struct ClosureRecordDistributor {
    ClosureRecordDistributor() = delete;
    ClosureRecordDistributor(LambdaCPS* const lambda_arg);

    void add_child(LambdaCPS* const child);
    void build_clsr_record_factory();
    void distribute();

private:
    LambdaCPS* lambda;
    std::unique_ptr<ClosureRecordFactory> crf;
    std::vector<LambdaCPS*> children;
};

struct ExternRefsCollector : public ModifyCPSVisitor {
    ExternRefsCollector(LambdaCPS* const lambda);

    virtual void visit(LambdaCPS* const cps) override;
    virtual void visit(PrimitiveCPS* const cps) override;
    virtual void visit(ApplyCPS* const cps) override;
    virtual void visit(BindCPS* const cps) override;
    virtual void visit(VarCPS* const cps) override;
    virtual void visit(ConstantCPS* const cps) override;

private:
    std::set<std::string> inner, outer;
    LambdaCPS *root;
    int lambda_cnt;

    bool is_inner_var(const std::string &name) const noexcept;

    friend void compiler::set_ext_refs(LambdaCPS* const);
};

struct ClosureTranslator : public ModifyCPSVisitor {
    virtual void visit(LambdaCPS* const cps) override;
    virtual void visit(PrimitiveCPS* const cps) override;
    virtual void visit(ApplyCPS* const cps) override;
    virtual void visit(BindCPS* const cps) override;
    virtual void visit(VarCPS* const cps) override;
    virtual void visit(ConstantCPS* const cps) override;

private:
    const LexicalScope *lex_scope = nullptr;
};

}

}

#endif

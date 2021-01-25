#ifndef INCLUDE_COMPILER_CLOSURE
#define INCLUDE_COMPILER_CLOSURE

#include <memory>
#include "cps.hpp"

namespace compiler {

void set_lex_scope(LambdaCPS* const lambda);
void closure_translation(CPSNode* const root);

struct CollectExternRefs : public ModifyCPSVisitor {
    virtual void visit(LambdaCPS* const cps) override;
    virtual void visit(PrimitiveCPS* const cps) override;
    virtual void visit(ApplyCPS* const cps) override;
    virtual void visit(BindCPS* const cps) override;
    virtual void visit(VarCPS* const cps) override;
    virtual void visit(ConstantCPS* const cps) override;

private:
    std::set<std::string> inner, outer;
    bool is_inner_var(const std::string &name) const noexcept;
    friend void set_closure(LambdaCPS* const);
};

struct ClosureTranslator : public ModifyCPSVisitor {
    virtual void visit(LambdaCPS* const cps) override;
    virtual void visit(PrimitiveCPS* const cps) override;
    virtual void visit(ApplyCPS* const cps) override;
    virtual void visit(BindCPS* const cps) override;
    virtual void visit(VarCPS* const cps) override;
    virtual void visit(ConstantCPS* const cps) override;

private:
    LexicalScope *lex_scope = nullptr;
};

}

#endif

#ifndef INCLUDE_CPS
#define INCLUDE_CPS

#include "ast.hpp"

namespace compiler {

struct CPSVisitor;

struct CPSNode {
    using node_ptr = CPSNode*;
    using const_node_ptr = CPSNode* const;
    virtual ~CPSNode();
    virtual void accept(CPSVisitor &visitor) const = 0;
};

struct LambdaCPS : public CPSNode {
    LambdaCPS(std::vector<std::string> args_arg, node_ptr body_arg);
    virtual ~LambdaCPS() override;
    const std::string& get_arg(const std::size_t i) const noexcept;
    std::size_t get_arg_num() const noexcept;
    const_node_ptr get_body() const noexcept;
    virtual void accept(CPSVisitor &visitor) const override;

private:
    std::vector<std::string> args;
    node_ptr body;
};

struct PrimitiveCPS : public CPSNode {
    enum class Type {
        ADD, DEC, AND, OR,
    };

    PrimitiveCPS() = delete;
    PrimitiveCPS(const Type type_arg);
    virtual ~PrimitiveCPS() override;
    static PrimitiveCPS* try_make(const std::string &s);
    const Type get_type() const noexcept;
    virtual void accept(CPSVisitor &visitor) const override;

private:
    Type type;
};

struct ApplyCPS : public CPSNode {
    ApplyCPS(node_ptr proc_arg, std::vector<node_ptr> args_arg);
    virtual ~ApplyCPS() override;
    const_node_ptr get_proc() const noexcept;
    const_node_ptr get_arg(const std::size_t i) const noexcept;
    std::size_t get_arg_num() const noexcept;
    virtual void accept(CPSVisitor &visitor) const override;

private:
    node_ptr proc;
    std::vector<node_ptr> args;
};

struct VarCPS : public CPSNode {
    VarCPS(std::string var_arg);
    virtual ~VarCPS();
    const std::string& get_var() const noexcept;
    virtual void accept(CPSVisitor &visitor) const override;

private:
    std::string var;
};

struct ConstantCPS : public CPSNode {
    ConstantCPS(const std::int32_t c_arg);
    virtual ~ConstantCPS();
    std::int32_t get_value() const noexcept;
    virtual void accept(CPSVisitor &visitor) const override;

private:
    std::int32_t c;
};

struct AST2CPS : public ASTNodeVisitor {
    CPSNode *res = nullptr;

    virtual void visit(const EvalNode* const node) override;
    virtual void visit(const LambdaNode* const node) override;
    virtual void visit(const SymbolNode* const node) override;
    virtual void visit(const ConstantNode* const node) override;
    virtual void visit(const SequenceNode* const node) override;
};

struct CPSVisitor {
    virtual void visit(const LambdaCPS* const cps) = 0;
    virtual void visit(const PrimitiveCPS* const cps) = 0;
    virtual void visit(const ApplyCPS* const cps) = 0;
    virtual void visit(const VarCPS* const cps) = 0;
    virtual void visit(const ConstantCPS* const cps) = 0;
};

struct PrintCPS : public CPSVisitor {
    virtual void visit(const LambdaCPS* const cps) override;
    virtual void visit(const PrimitiveCPS* const cps) override;
    virtual void visit(const ApplyCPS* const cps) override;
    virtual void visit(const VarCPS* const cps) override;
    virtual void visit(const ConstantCPS* const cps) override;

private:
    int nest = 0;
    void print_nest();
};

}

#endif

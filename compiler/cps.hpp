#ifndef INCLUDE_COMPILER_CPS
#define INCLUDE_COMPILER_CPS

#include <fstream>
#include "ast.hpp"
#include "lexical_scope.hpp"

namespace compiler {

namespace internal {

struct DependencyAnalyzer;
struct ClosureRecordSetter; 

}

struct CPSVisitor;
struct ModifyCPSVisitor;

struct CPSNode {
    using node_ptr = CPSNode*;
    using const_node_ptr = CPSNode* const;
    virtual ~CPSNode();
    virtual void accept(CPSVisitor &visitor) const = 0;
    virtual void accept(ModifyCPSVisitor &visitor) = 0;
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
    virtual void accept(ModifyCPSVisitor &visitor) override;

private:
    Type type;
};

struct ApplyCPS : public CPSNode {
    ApplyCPS(node_ptr proc_arg, std::vector<node_ptr> args_arg);
    virtual ~ApplyCPS() override;
    node_ptr get_proc() noexcept;
    node_ptr get_arg(const std::size_t i) noexcept;
    const_node_ptr get_proc() const noexcept;
    const_node_ptr get_arg(const std::size_t i) const noexcept;
    std::size_t get_arg_num() const noexcept;
    virtual void accept(CPSVisitor &visitor) const override;
    virtual void accept(ModifyCPSVisitor &visitor) override;

    template <typename F>
    void for_each_args(F f) {
        for (std::size_t i = 0; i != args.size(); i++) f(args[i], i);
    }
    
    template <typename F>
    void for_each_args(F f) const {
        for (std::size_t i = 0; i != args.size(); i++) f(args[i], i);
    }

private:
    node_ptr proc;
    std::vector<node_ptr> args;
};

struct BindCPS : public CPSNode {
    BindCPS(std::string name_arg, node_ptr value_arg);
    virtual ~BindCPS() override;
    const std::string& get_name() const noexcept;
    node_ptr get_value() noexcept;
    const_node_ptr get_value() const noexcept;
    virtual void accept(CPSVisitor &visitor) const override;
    virtual void accept(ModifyCPSVisitor &visitor) override;

private:
    std::string name;
    node_ptr value;
};

struct LambdaCPS : public CPSNode {
    using bind_ptr = BindCPS*;
    using const_bind_ptr = const BindCPS*;

    LambdaCPS(std::vector<std::string> args_arg,
              std::vector<bind_ptr> binds_arg,
              node_ptr body_arg);
    LambdaCPS(std::vector<std::string> args_arg,
              node_ptr body_arg);

    virtual ~LambdaCPS() override;
    
    const std::string& get_arg(const std::size_t i) const;
    const_bind_ptr get_bind(const std::size_t i) const;
    bind_ptr get_bind(const std::size_t i);
    const std::string& get_ext_ref(const std::size_t i) const;
    
    std::size_t get_arg_num() const noexcept;
    std::size_t get_bind_num() const noexcept;
    std::size_t get_ext_ref_num() const noexcept;

    const_node_ptr get_body() const noexcept;
    node_ptr get_body() noexcept;
    const std::vector<std::string>& get_raw_ext_refs() const noexcept;
    const LexicalScope* get_lex_scope() const noexcept;
    const refs_seq_type* get_passing_record() const noexcept;

    void set_clsr_record(const ClosureRecord clsr_record) noexcept;

    virtual void accept(CPSVisitor &visitor) const override;
    virtual void accept(ModifyCPSVisitor &visitor) override;

    template <typename F>
    void for_each_args(F f) {
        for (std::size_t i = 0; i != args.size(); i++) f(args[i], i);
    }

    template <typename F>
    void for_each_args(F f) const {
        for (std::size_t i = 0; i != args.size(); i++) f(args[i], i);
    }

    template <typename F>
    void for_each_binds(F f) {
        for (std::size_t i = 0; i != binds.size(); i++) f(binds[i], i);
    }

    template <typename F>
    void for_each_binds(F f) const {
        for (std::size_t i = 0; i != binds.size(); i++) f(binds[i], i);
    }

private:
    refs_seq_type args, locals, ext_refs;
    std::vector<bind_ptr> binds;
    node_ptr body;
    std::unique_ptr<LexicalScope> lex_scope;
    std::unique_ptr<refs_seq_type> to_pass;

    friend struct compiler::internal::DependencyAnalyzer;
    friend struct compiler::internal::ClosureRecordSetter;
};

struct VarCPS : public CPSNode {
    VarLocation ref;
    
    VarCPS(std::string var_arg);
    virtual ~VarCPS();
    const std::string& get_var() const noexcept;
    virtual void accept(CPSVisitor &visitor) const override;
    virtual void accept(ModifyCPSVisitor &visitor) override;

private:
    std::string var;
};

struct ConstantCPS : public CPSNode {
    ConstantCPS(const std::int32_t c_arg);
    virtual ~ConstantCPS();
    std::int32_t get_value() const noexcept;
    virtual void accept(CPSVisitor &visitor) const override;
    virtual void accept(ModifyCPSVisitor &visitor) override;

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
    virtual void visit(const BindNode* const node) override;
};

struct CPSVisitor {
    virtual void visit(const LambdaCPS* const cps) = 0;
    virtual void visit(const PrimitiveCPS* const cps) = 0;
    virtual void visit(const ApplyCPS* const cps) = 0;
    virtual void visit(const BindCPS* const cps) = 0;
    virtual void visit(const VarCPS* const cps) = 0;
    virtual void visit(const ConstantCPS* const cps) = 0;
};

struct ModifyCPSVisitor {
    virtual void visit(LambdaCPS* const cps) = 0;
    virtual void visit(PrimitiveCPS* const cps) = 0;
    virtual void visit(ApplyCPS* const cps) = 0;
    virtual void visit(BindCPS* const cps) = 0;
    virtual void visit(VarCPS* const cps) = 0;
    virtual void visit(ConstantCPS* const cps) = 0;
};

void print_cps_code(const std::string &filename, const CPSNode* const cps);

}

#endif

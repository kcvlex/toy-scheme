#ifndef INCLUDE_AST
#define INCLUDE_AST

#include <vector>
#include <string>

namespace compiler {

struct EvalNode;
struct LambdaNode;
struct ConstantNode;
struct SymbolNode;
struct SequenceNode;
// struct DefineNode;

struct ASTNodeVisitor;


/********** AST Node **********/

struct ASTNode {
    using node_ptr = ASTNode*;
    virtual ~ASTNode();
    virtual void accept(ASTNodeVisitor &visitor) const = 0;
};

struct EvalNode : public ASTNode {
    EvalNode();
    virtual ~EvalNode() override;

    virtual void accept(ASTNodeVisitor &visitor) const override;
    const std::vector<node_ptr>& get_children() const;
    void add_child(node_ptr node);
    std::size_t size() const noexcept;

private:
    std::vector<node_ptr> children;
};

struct LambdaNode : public ASTNode {
    LambdaNode(std::vector<SymbolNode*> args_arg, node_ptr body_arg);
    virtual ~LambdaNode() override;

    virtual void accept(ASTNodeVisitor &visitor) const override;
    const std::vector<SymbolNode*>& get_args() const;
    std::size_t arg_size() const noexcept;
    const node_ptr get_body() const;

private:
    std::vector<SymbolNode*> args;
    node_ptr body;
};

struct SymbolNode : public ASTNode {
    SymbolNode(std::string symbol_arg);
    virtual ~SymbolNode() override;

    virtual void accept(ASTNodeVisitor &visitor) const override;
    const std::string& get_symbol() const;

private:
    std::string symbol;
};

// Only integer
struct ConstantNode : public ASTNode {
    ConstantNode(int val_arg);
    virtual ~ConstantNode() override;

    virtual void accept(ASTNodeVisitor &visitor) const override;
    int get_value() const;

private:
    int val;
};

struct SequenceNode : public ASTNode {
    using seq_type = std::vector<ASTNode*>;

    SequenceNode(seq_type seq_arg);
    virtual ~SequenceNode() override;

    virtual void accept(ASTNodeVisitor &visitor) const override;
    const seq_type& get_seq() const noexcept;

private:
    seq_type seq;
};


/********** Node Visitor **********/

struct ASTNodeVisitor {
    virtual void visit(const EvalNode* const node) = 0;
    virtual void visit(const LambdaNode* const node) = 0;
    virtual void visit(const SymbolNode* const node) = 0;
    virtual void visit(const ConstantNode* const node) = 0;
    virtual void visit(const SequenceNode* const node) = 0;
};

struct DebugASTNodeVisitor : public ASTNodeVisitor {
    DebugASTNodeVisitor(int depth_arg);
    DebugASTNodeVisitor();
    
    virtual void visit(const EvalNode* const node) override;
    virtual void visit(const LambdaNode* const node) override;
    virtual void visit(const SymbolNode* const node) override;
    virtual void visit(const ConstantNode* const node) override;
    virtual void visit(const SequenceNode* const node) override;

private:
    int depth;

    void write_lines();
};

}

#endif

#ifndef INCLUDE_AST
#define INCLUDE_AST

#include <vector>
#include <string>

namespace compiler {

struct EvalNode;
struct LambdaNode;
struct ConstantNode;
struct SymbolNode;
// struct DefineNode;

struct ConstNodeVisitor;
struct NodeVisitor;

struct ASTNode {
    using node_ptr = ASTNode*;
    virtual ~ASTNode();
    virtual void accept(NodeVisitor &visitor) = 0;
    virtual void accept(ConstNodeVisitor &visitor) const = 0;
};

struct EvalNode : public ASTNode {
    EvalNode();
    virtual ~EvalNode() override;

    virtual void accept(NodeVisitor &visitor) override;
    virtual void accept(ConstNodeVisitor &visitor) const override;
    const std::vector<node_ptr>& get_children() const;
    void add_child(node_ptr node);
    std::size_t size() const noexcept;

private:
    std::vector<node_ptr> children;
};

struct LambdaNode : public ASTNode {
    LambdaNode(std::vector<SymbolNode*> args_arg, node_ptr body_arg);
    virtual ~LambdaNode() override;

    virtual void accept(NodeVisitor &visitor) override;
    virtual void accept(ConstNodeVisitor &visitor) const override;
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

    virtual void accept(NodeVisitor &visitor) override;
    virtual void accept(ConstNodeVisitor &visitor) const override;
    const std::string& get_symbol() const;

private:
    std::string symbol;
};

// Only integer
struct ConstantNode : public ASTNode {
    ConstantNode(int val_arg);
    virtual ~ConstantNode() override;

    virtual void accept(NodeVisitor &visitor) override;
    virtual void accept(ConstNodeVisitor &visitor) const override;
    int get_value() const;

private:
    int val;
};

struct NodeVisitor {
    virtual void visit(const EvalNode *node) = 0;
    virtual void visit(const LambdaNode *node) = 0;
    virtual void visit(const SymbolNode *node) = 0;
    virtual void visit(const ConstantNode *node) = 0;
};

struct ConstNodeVisitor {
    virtual void visit(const EvalNode* const node) = 0;
    virtual void visit(const LambdaNode* const node) = 0;
    virtual void visit(const SymbolNode* const node) = 0;
    virtual void visit(const ConstantNode* const node) = 0;
};

struct DebugConstNodeVisitor : public ConstNodeVisitor {
    DebugConstNodeVisitor(int depth_arg);
    DebugConstNodeVisitor();
    
    virtual void visit(const EvalNode* const node) override;
    virtual void visit(const LambdaNode* const node) override;
    virtual void visit(const SymbolNode* const node) override;
    virtual void visit(const ConstantNode* const node) override;

private:
    int depth;

    void write_lines();
};

}

#endif

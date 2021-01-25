#ifndef INCLUDE_COMPILER_AST
#define INCLUDE_COMPILER_AST

#include <vector>
#include <string>

namespace compiler {

struct EvalNode;
struct LambdaNode;
struct ConstantNode;
struct SymbolNode;
struct SequenceNode;
struct BindNode;

struct ASTNodeVisitor;


/********** AST Node **********/

struct ASTNode {
    using node_ptr = ASTNode*;
    using const_node_ptr = ASTNode* const;
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
    LambdaNode(std::vector<std::string> args_arg,
               std::vector<node_ptr> bodies_arg);
    virtual ~LambdaNode() override;

    virtual void accept(ASTNodeVisitor &visitor) const override;
    const std::string& get_arg(const std::size_t i) const;
    const_node_ptr get_body(const std::size_t i) const;
    std::size_t get_arg_num() const noexcept;
    std::size_t get_body_num() const noexcept;

private:
    std::vector<std::string> args;
    std::vector<node_ptr> bodies;
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
    int get_value() const noexcept;

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

struct BindNode : public ASTNode {
    BindNode(std::string name_arg, node_ptr value_arg);
    virtual ~BindNode() override;

    virtual void accept(ASTNodeVisitor &visitor) const override;
    const std::string& get_name() const noexcept;
    const_node_ptr get_value() const noexcept;

private:
    std::string name;
    node_ptr value;
};


/********** Node Visitor **********/

struct ASTNodeVisitor {
    virtual void visit(const EvalNode* const node) = 0;
    virtual void visit(const LambdaNode* const node) = 0;
    virtual void visit(const SymbolNode* const node) = 0;
    virtual void visit(const ConstantNode* const node) = 0;
    virtual void visit(const SequenceNode* const node) = 0;
    virtual void visit(const BindNode* const node) = 0;
};

struct DebugASTNodeVisitor : public ASTNodeVisitor {
    DebugASTNodeVisitor(int depth_arg);
    DebugASTNodeVisitor();
    
    virtual void visit(const EvalNode* const node) override;
    virtual void visit(const LambdaNode* const node) override;
    virtual void visit(const SymbolNode* const node) override;
    virtual void visit(const ConstantNode* const node) override;
    virtual void visit(const SequenceNode* const node) override;
    virtual void visit(const BindNode* const node) override;

private:
    int depth;

    void write_lines();
};

}

#endif

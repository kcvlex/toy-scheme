#include "ast.hpp"
#include <iostream>

namespace compiler {

/***** For Visitor *****/

void EvalNode::accept(ASTNodeVisitor &visitor) const { visitor.visit(this); }
void LambdaNode::accept(ASTNodeVisitor &visitor) const { visitor.visit(this); }
void SymbolNode::accept(ASTNodeVisitor &visitor) const { visitor.visit(this); }
void ConstantNode::accept(ASTNodeVisitor &visitor) const { visitor.visit(this); }
void SequenceNode::accept(ASTNodeVisitor &visitor) const { visitor.visit(this); }


/***** AST Node *****/

ASTNode::~ASTNode() {
}


/***** Eval Node *****/

EvalNode::EvalNode() : children()
{
}

EvalNode::~EvalNode() {
    for (auto ch : children) delete ch;
}


const std::vector<ASTNode::node_ptr>& EvalNode::get_children() const {
    return children;
}

void EvalNode::add_child(node_ptr node) {
    children.push_back(node);
}

std::size_t EvalNode::size() const noexcept {
    return children.size();
}


/***** Lambda Node *****/

LambdaNode::LambdaNode(std::vector<SymbolNode*> args_arg, node_ptr body_arg)
    : args(std::move(args_arg)), body(body_arg)
{
}

LambdaNode::~LambdaNode() {
    for (auto arg : args) delete arg;
    delete body;
}

const std::vector<SymbolNode*>& LambdaNode::get_args() const {
    return args;
}

std::size_t LambdaNode::arg_size() const noexcept {
    return args.size();
}

const ASTNode::node_ptr LambdaNode::get_body() const {
    return body;
}


/***** Symbol Node *****/

SymbolNode::SymbolNode(std::string symbol_arg)
    : symbol(std::move(symbol_arg))
{
}

SymbolNode::~SymbolNode() {
}

const std::string& SymbolNode::get_symbol() const {
    return symbol;
}


/***** Constant Node *****/

ConstantNode::ConstantNode(int val_arg) : val(val_arg) 
{
}

ConstantNode::~ConstantNode() {
}

int ConstantNode::get_value() const {
    return val;
}


/***** Sequence Node *****/

SequenceNode::SequenceNode(std::vector<ASTNode*> seq_arg) : seq(std::move(seq_arg)) 
{
}

SequenceNode::~SequenceNode() {
    for (auto expr : seq) delete expr;
}

const SequenceNode::seq_type& SequenceNode::get_seq() const noexcept {
    return seq;
}


/***** Visitor For Debug *****/

DebugASTNodeVisitor::DebugASTNodeVisitor(int depth_arg)
    : depth(depth_arg)
{
}

DebugASTNodeVisitor::DebugASTNodeVisitor()
    : DebugASTNodeVisitor(0)
{
}

void DebugASTNodeVisitor::visit(const EvalNode* const node) {
    write_lines();
    std::cout << "- EvalNode" << std::endl;
    depth++;
    decltype(auto) children = node->get_children();
    for (auto ch : children) ch->accept(*this);
    depth--;
}

void DebugASTNodeVisitor::visit(const LambdaNode* const node) {
    write_lines();
    std::cout << "- LambdaNode" << std::endl;
    depth++;
    decltype(auto) args = node->get_args();
    for (auto arg : args) arg->accept(*this);
    write_lines();
    std::cout << std::endl;
    node->get_body()->accept(*this);
    depth--;
}

void DebugASTNodeVisitor::visit(const SymbolNode* const node) {
    write_lines();
    std::cout << "- SymbolNode(" << node->get_symbol() << ")\n";
}

void DebugASTNodeVisitor::visit(const ConstantNode* const node) {
    write_lines();
    std::cout << "- ConstantNode(" << node->get_value() << ")\n";
}

void DebugASTNodeVisitor::visit(const SequenceNode* const node) {
    write_lines();
    std::cout << "- SequenceNode\n";
    depth++;
    for (auto expr : node->get_seq()) expr->accept(*this);
    depth--;
}

void DebugASTNodeVisitor::write_lines() {
    for (int i = 0; i < depth; i++) std::cout << "|";
}

}

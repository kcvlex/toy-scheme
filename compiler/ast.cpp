#include "ast.hpp"
#include <iostream>

namespace compiler {

/***** For Visitor *****/

void EvalNode::accept(NodeVisitor &visitor) { visitor.visit(this); }
void LambdaNode::accept(NodeVisitor &visitor) { visitor.visit(this); }
void SymbolNode::accept(NodeVisitor &visitor) { visitor.visit(this); }
void ConstantNode::accept(NodeVisitor &visitor) { visitor.visit(this); }

void EvalNode::accept(ConstNodeVisitor &visitor) const { visitor.visit(this); }
void LambdaNode::accept(ConstNodeVisitor &visitor) const { visitor.visit(this); }
void SymbolNode::accept(ConstNodeVisitor &visitor) const { visitor.visit(this); }
void ConstantNode::accept(ConstNodeVisitor &visitor) const { visitor.visit(this); }


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


/***** Visitor For Debug *****/

DebugConstNodeVisitor::DebugConstNodeVisitor(int depth_arg)
    : depth(depth_arg)
{
}

DebugConstNodeVisitor::DebugConstNodeVisitor()
    : DebugConstNodeVisitor(0)
{
}

void DebugConstNodeVisitor::visit(const EvalNode* const node) {
    write_lines();
    std::cout << "- EvalNode" << std::endl;
    depth++;
    decltype(auto) children = node->get_children();
    for (auto ch : children) ch->accept(*this);
    depth--;
}

void DebugConstNodeVisitor::visit(const LambdaNode* const node) {
    write_lines();
    std::cout << "- lambda" << std::endl;
    depth++;
    decltype(auto) args = node->get_args();
    for (auto arg : args) arg->accept(*this);
    write_lines();
    std::cout << std::endl;
    node->get_body()->accept(*this);
    depth--;
}

void DebugConstNodeVisitor::visit(const SymbolNode* const node) {
    write_lines();
    std::cout << "- SymbolNode(" << node->get_symbol() << ")\n";
}

void DebugConstNodeVisitor::visit(const ConstantNode* const node) {
    write_lines();
    std::cout << "- ConstantNode(" << node->get_value() << ")\n";
}

void DebugConstNodeVisitor::write_lines() {
    for (int i = 0; i < depth; i++) std::cout << "|";
}

}

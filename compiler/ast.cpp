#include "ast.hpp"
#include <iostream>
#include <utility>

namespace compiler {

/***** For Visitor *****/

void EvalNode::accept(ASTNodeVisitor &visitor) const { visitor.visit(this); }
void LambdaNode::accept(ASTNodeVisitor &visitor) const { visitor.visit(this); }
void SymbolNode::accept(ASTNodeVisitor &visitor) const { visitor.visit(this); }
void ConstantNode::accept(ASTNodeVisitor &visitor) const { visitor.visit(this); }
void SequenceNode::accept(ASTNodeVisitor &visitor) const { visitor.visit(this); }
void BindNode::accept(ASTNodeVisitor &visitor) const { visitor.visit(this); }


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

LambdaNode::LambdaNode(std::vector<std::string> args_arg, 
                       std::vector<node_ptr> bodies_arg)
    : args(std::move(args_arg)), 
      bodies(std::move(bodies_arg))
{
}

LambdaNode::~LambdaNode() {
    for (auto body : bodies) delete body;
}

const std::string& LambdaNode::get_arg(const std::size_t i) const {
    return args[i];
}

ASTNode::const_node_ptr LambdaNode::get_body(const std::size_t i) const {
    return bodies[i];
}

std::size_t LambdaNode::get_arg_num() const noexcept {
    return args.size();
}

std::size_t LambdaNode::get_body_num() const noexcept {
    return bodies.size();
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

int ConstantNode::get_value() const noexcept {
    return val;
}


/***** Sequence Node *****/

SequenceNode::SequenceNode(std::vector<ASTNode*> seq_arg) 
    : seq(std::move(seq_arg)) 
{
}

SequenceNode::~SequenceNode() {
    for (auto expr : seq) delete expr;
}

const SequenceNode::seq_type& SequenceNode::get_seq() const noexcept {
    return seq;
}


/***** Bind Node *****/

BindNode::BindNode(std::string name_arg, node_ptr value_arg)
    : name(std::move(name_arg)),
      value(value_arg)
{
}

BindNode::~BindNode() {
    delete value;
}

const std::string& BindNode::get_name() const noexcept {
    return name;
}

ASTNode::const_node_ptr BindNode::get_value() const noexcept {
    return value;
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
    std::cout << "- LambdaNode(";
    for (std::size_t i = 0; i != node->get_arg_num(); i++) {
        std::cout << node->get_arg(i) << ',';
    }
    std::cout << ')' << std::endl;
    
    depth++;
    for (std::size_t i = 0; i != node->get_body_num(); i++) {
        node->get_body(i)->accept(*this);
    }
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

void DebugASTNodeVisitor::visit(const BindNode* const node) {
    write_lines();
    std::cout << "- BindNode(" << node->get_name() << ")\n";
    depth++;
    node->get_value()->accept(*this);
    depth--;
}

void DebugASTNodeVisitor::write_lines() {
    for (int i = 0; i < depth; i++) std::cout << "|";
}

}

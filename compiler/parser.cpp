#include "parser.hpp"
#include <iostream>

namespace compiler {

namespace {

const std::string lambda_symbol = "lambda";
const std::string begin_symbol = "begin";
const std::string define_symbol = "define";
const std::string l_paren = "(";
const std::string r_paren = ")";

bool is_dig(char c) {
    return '0' <= c && c <= '9';
}

}

Parser::Parser(TokenStream stream) : stream(std::move(stream))
{
}

bool Parser::lookup1(const std::string &s) const noexcept {
    return 2 <= stream.rest_size() &&
           *stream.lookup(0) == l_paren &&
           *stream.lookup(1) == s;
}

bool Parser::is_lambda() const noexcept {
    return lookup1(lambda_symbol);
}

bool Parser::is_begin() const noexcept {
    return lookup1(begin_symbol);
}

bool Parser::is_define() const noexcept {
    return lookup1(define_symbol);
}

SequenceNode* Parser::parse() {
    return parse_seq();
}

ASTNode* Parser::parse_expr() {
    if (is_lambda()) return parse_lambda();
    if (is_begin()) return parse_seq();
    if (is_define()) return parse_bind();
    if (*stream.head() == l_paren) return parse_eval();
    if (is_dig((*stream.head())[0])) return parse_constant();
    return parse_symbol();
}

EvalNode* Parser::parse_eval() {
    stream.eat(l_paren);
    EvalNode *res = new EvalNode();
    res->add_child(parse_expr());
    
    while (!stream.finished()) {
        if (*stream.head() == r_paren) break;
        res->add_child(parse_expr());
    }

    stream.eat(r_paren);
    return res;
}

LambdaNode* Parser::parse_lambda() {
    stream.eat(l_paren)
          .eat(lambda_symbol)
          .eat(l_paren);
    std::vector<std::string> symbols;
    while (!stream.finished()) {
        if (*stream.head() == r_paren) break;
        symbols.push_back(*stream.advance());
    }
    stream.eat(r_paren);  // end args

    std::vector<ASTNode*> bodies;
    while (!stream.finished()) {
        if (*stream.head() == r_paren) break;
        bodies.push_back(parse_expr());
    }

    stream.eat(r_paren);  // end lambda
    return new LambdaNode(std::move(symbols), bodies);
}

SymbolNode* Parser::parse_symbol() {
    return new SymbolNode(*stream.advance());
}

ConstantNode* Parser::parse_constant() {
    auto ite = stream.advance();
    return new ConstantNode(std::stoi(*ite));
}

SequenceNode* Parser::parse_seq() {
    stream.eat(l_paren)
          .eat(begin_symbol);
    std::vector<ASTNode*> seq;
    while (!stream.finished()) {
        if (*stream.head() == r_paren) break;
        seq.push_back(parse_expr());
    }
    stream.eat(r_paren);
    return new SequenceNode(std::move(seq));
}

BindNode* Parser::parse_bind() {
    stream.eat(l_paren)
          .eat(define_symbol);
    auto name = *stream.advance();
    auto value = parse_expr();
    stream.eat(r_paren);
    return new BindNode(std::move(name), value);
}

}

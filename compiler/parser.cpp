#include "parser.hpp"
#include <iostream>

namespace compiler {

namespace {

const std::string lambda_symbol = "lambda";
const std::string begin_symbol = "begin";
const std::string l_paren = "(";
const std::string r_paren = ")";

bool is_dig(char c) {
    return '0' <= c && c <= '9';
}

}

Parser::Parser(TokenStream stream) : stream(std::move(stream))
{
}

bool Parser::is_lambda() const {
    return 2 <= stream.rest_size() &&
           *stream.lookup(0) == l_paren &&
           *stream.lookup(1) == lambda_symbol;
}

bool Parser::is_begin() const {
    return 2 <= stream.rest_size() &&
           *stream.lookup(0) == l_paren &&
           *stream.lookup(1) == begin_symbol;
}

SequenceNode* Parser::parse() {
    return parse_seq();
}

ASTNode* Parser::parse_expr() {
    if (is_lambda()) return parse_lambda();
    if (is_begin()) return parse_seq();
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
    std::vector<SymbolNode*> symbols;
    while (!stream.finished()) {
        if (*stream.head() == r_paren) break;
        symbols.push_back(parse_symbol());
    }
    stream.eat(r_paren);  // end args
    auto body = parse_expr();
    stream.eat(r_paren);  // end lambda
    return new LambdaNode(std::move(symbols), body);
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

}

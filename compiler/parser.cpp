#include "parser.hpp"
#include <iostream>

namespace compiler {

namespace {

const std::string lambda_symbol = "lambda";
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
    return *stream.lookup(0) == l_paren &&
           *stream.lookup(1) == lambda_symbol;
}

ASTNode* Parser::parse() {
    return parse_eval();
    /*
    auto ret = new EvalNode();
    while (!stream.finished()) {
        if (is_lambda()) {
            ret->add_child(parse_lambda());
        } else {
            ret->add_child(parse_eval());
        }
    }
    return ret;
    */
}

EvalNode* Parser::parse_eval() {
    stream.eat(l_paren);
    EvalNode *res = new EvalNode();
    while (!stream.finished()) {
        if (*stream.head() == r_paren) break;
        if (is_lambda()) {
            res->add_child(parse_lambda());
        } else if (*stream.head() == l_paren) {
            res->add_child(parse_eval());
        } else if (is_dig((*stream.head())[0])) {
            res->add_child(parse_constant());
        } else {
            res->add_child(parse_symbol());
        }
    }
    stream.eat(r_paren);
    return res;
}

LambdaNode* Parser::parse_lambda() {
    stream.eat(l_paren);
    stream.eat(lambda_symbol);
    stream.eat(l_paren);
    std::vector<SymbolNode*> symbols;
    while (!stream.finished()) {
        if (*stream.head() == r_paren) break;
        symbols.push_back(parse_symbol());
    }
    stream.eat(r_paren);  // end args
    auto body = parse_eval();
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

}

#include <iostream>
#include "token.hpp"
#include "parser.hpp"

void test_tokenize(const std::string &s) {
    compiler::TokenStream ts = std::move(compiler::Tokenizer::build(s));
    while (!ts.finished()) std::cout << *ts.advance() << std::endl;
}

void test_parser_and_visitor(const std::string &s) {
    compiler::TokenStream ts = std::move(compiler::Tokenizer::build(s));
    compiler::Parser parser(std::move(ts));
    auto root = parser.parse();
    compiler::DebugConstNodeVisitor visitor;
    root->accept(visitor);
    delete root;
}

int main() {
    std::string code = "(lambda (f a b) \n  (+ a b 1 2))\n";
    test_parser_and_visitor(code);
    return 0;
}

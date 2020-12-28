#include <iostream>
#include "token.hpp"
#include "parser.hpp"
#include "three_address_code.hpp"
#include "sem_analyzer.hpp"
#include "util/enum2str.hpp"

using namespace compiler;

void test_tokenize(const std::string &s) {
    TokenStream ts = std::move(Tokenizer::build(s));
    while (!ts.finished()) std::cout << *ts.advance() << std::endl;
}

ASTNode* test_parser_and_visitor(const std::string &s) {
    TokenStream ts = std::move(Tokenizer::build(s));
    Parser parser(std::move(ts));
    auto root = parser.parse();
    DebugConstNodeVisitor visitor;
    root->accept(visitor);
    return root;
}

void test_sem_analyzer(std::vector<ASTNode*> nodes) {
    SemanticAnalyzer s_analy;
    auto res = s_analy.analyze(nodes);
    for (auto func : res) {
        auto [ label, is ] = func;
        auto os = std::move(is.convert());
        std::cout << label << ":" << std::endl;
        while (!os.finished()) {
            auto ite = os.get();
            os.advance();
            std::cout << *ite << std::endl;
        }
    }
}

int main() {
    std::string code = "(lambda (f a b) \n  (+ a (+ b (+ 1 2))))\n";
    auto root = test_parser_and_visitor(code);
    std::vector<ASTNode*> nodes = { root };
    test_sem_analyzer(nodes);
    delete root;
    return 0;
}

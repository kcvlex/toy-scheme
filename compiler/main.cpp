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

void test_regarrays() {
    for (auto e : regs::callee_saved_regs) std::cout << int(e) << ", ";
    std::cout << std::endl;
    for (auto e : regs::argument_regs) std::cout << int(e) << ", ";
    std::cout << std::endl;
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
            std::cout << util::to_str<Instructions>(ite->instr);
            std::string elim = "\t";
            for (auto op : { ite->op1, ite->op2, ite->op3 }) {
                if (!op.has_value()) break;
                std::cout << std::exchange(elim, ",");
                auto value = op.value();
                if (auto p = std::get_if<0>(&value)) {
                    std::cout << "x" << int(*p);
                } else if (auto p = std::get_if<1>(&value)) {
                    std::cout << int(*p);
                } else if (auto p = std::get_if<2>(&value)) {
                    auto [ reg, imm ] = *p;
                    std::cout << imm << "(x" << int(reg) << ")";
                }
            }
            std::cout << std::endl;
        }
    }
}

int main() {
    test_regarrays();
    std::string code = "(lambda (f a b) \n  (+ a (+ b (+ 1 2))))\n";
    auto root = test_parser_and_visitor(code);
    std::vector<ASTNode*> nodes = { root };
    test_sem_analyzer(nodes);
    delete root;
    return 0;
}

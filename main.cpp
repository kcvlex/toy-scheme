#include <iostream>
#include <bitset>
#include "compiler/token.hpp"
#include "compiler/parser.hpp"
#include "compiler/three_address_code.hpp"
#include "compiler/sem_analyzer.hpp"
#include "assembler/assembler.hpp"
#include "util/enum2str.hpp"

using namespace compiler;
using namespace assembler;

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

auto test_sem_analyzer(std::vector<ASTNode*> nodes) {
    SemanticAnalyzer s_analy;
    auto res = s_analy.analyze(nodes);
    auto ret = res;
    for (auto func : res) {
        auto [ label, os ] = func;
        auto is = std::move(os.convert());
        std::cout << "LL" << label << ":" << std::endl;
        while (!is.finished()) {
            auto ite = is.get();
            is.advance();
            std::cout << *ite << std::endl;
        }
    }
    return ret;
}

void test_asm(std::vector<FunctionCode> fcodes) {
    assembler::Assembler assm(fcodes);
    auto res = assm.encode();
    std::cout << "p.imem.mem[0] = 32'd0;" << std::endl;
    std::cout << "p.imem.mem[1] = 32'd0;" << std::endl;
    for (std::size_t idx = 0; idx != res.size(); idx++) {
        std::cout << "p.imem.mem[" << idx + 2 << "] = 32'b" << std::bitset<32>(res[idx]) << ";" << std::endl;
    }
}

int main() {
    std::string code = "((lambda (f a b) \n  (+ a (+ b (+ 1 2)))) 42 43 44)\n";
    auto root = test_parser_and_visitor(code);
    std::vector<ASTNode*> nodes = { root };
    auto fcodes = test_sem_analyzer(nodes);
    test_asm(fcodes);
    delete root;
    return 0;
}

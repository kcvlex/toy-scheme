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
        auto [ label, is ] = func;
        auto os = std::move(is.convert());
        std::cout << label << ":" << std::endl;
        while (!os.finished()) {
            auto ite = os.get();
            os.advance();
            std::cout << *ite << std::endl;
        }
    }
    return ret;
}

void test_asm(std::vector<FunctionCode> fcodes) {
    // no jump
    auto [ label, is ] = fcodes[1];
    auto os = std::move(is.convert());

    assembler::Assembler assm;
    std::cout << "p.imem.mem[0] = 32'd0;" << std::endl;
    for (std::size_t idx = 1; !os.finished(); idx++) {
        auto ite = os.get();
        os.advance();
        auto res = assm.encode(*ite);
        std::cout << "p.imem.mem[" << idx << "] = 32'b" << std::bitset<32>(res) << ";" << std::endl;
    }
}

int main() {
    std::string code = "(lambda (f a b) \n  (+ a (+ b (+ 1 2))))\n";
    auto root = test_parser_and_visitor(code);
    std::vector<ASTNode*> nodes = { root };
    auto fcodes = test_sem_analyzer(nodes);
    test_asm(fcodes);
    delete root;
    return 0;
}

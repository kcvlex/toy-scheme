#include <iostream>
#include <bitset>
#include <limits>
#include "compiler/token.hpp"
#include "compiler/parser.hpp"
#include "compiler/closure.hpp"
#include "compiler/cps.hpp"
#include "util/enum2str.hpp"

#if 0
#include "assembler/assembler.hpp"
#include "compiler/three_address_code.hpp"
#include "compiler/sem_analyzer.hpp"
#endif

using namespace compiler;

#if 0
using namespace assembler;
#endif

void test_tokenize(const std::string &s) {
    TokenStream ts = std::move(Tokenizer::build(s));
    // while (!ts.finished()) std::cout << *ts.advance() << std::endl;
}

ASTNode* test_parser_and_visitor(const std::string &s) {
    TokenStream ts = std::move(Tokenizer::build(s));
    Parser parser(std::move(ts));
    auto root = parser.parse();
    DebugASTNodeVisitor visitor;
    root->accept(visitor);
    return root;
}

#if 0

auto test_sem_analyzer(const SequenceNode* const node) {
    SemanticAnalyzer s_analy;
    auto res = s_analy.analyze(node);
    auto ret = res;
    std::size_t line = 0;
    for (auto func : res) {
        auto [ label, os ] = func;
        auto is = std::move(os.convert());
        std::cout << "LL" << label << ":" << std::endl;
        while (!is.finished()) {
            auto ite = is.get();
            is.advance();
            std::cout << line << "\t" << *ite << std::endl;
            line += 4;
        }
    }
    return ret;
}

struct Printer {
    std::size_t idx = 0;

    ~Printer() {
        std::cout << "end\n"
                  << "always @(p.PC) begin if (p.PC[11:2] == 0) $finish(); end\n";
    }

    Printer& print(std::uint32_t instr) {
        std::cout << "p.imem.mem[" 
                  << idx++ 
                  << "] = 32'b" 
                  << std::bitset<32>(instr).to_string() 
                  << ";\n";
        return *this;
    }
};

void test_asm(std::vector<FunctionCode> fcodes) {
    assembler::Assembler assm(fcodes);
    auto res = assm.encode();
    Printer printer;
    for (std::size_t idx = 0; idx != res.size(); idx++) {
        printer.print(res[idx]);
    }
}

#endif

void test_cps(const ASTNode* const root) {
    AST2CPS visitor;
    root->accept(visitor);
    auto cps = visitor.res;
    closure_translation(cps);
    print_cps_code("tmp.test.scm", cps);
    delete cps;
}

// https://stackoverflow.com/questions/2602013/read-whole-ascii-file-into-c-stdstring
std::string read_file(const std::string &filename) {
    std::ifstream ifs(filename);
    std::string code;
    code.assign(std::istreambuf_iterator<char>(ifs),
                std::istreambuf_iterator<char>());
    ifs.close();
    return code;
}

int main() {
    std::string code = std::move(read_file("test1.scm"));
    auto root = test_parser_and_visitor(code);
    test_cps(root);
    /*
    auto fcodes = test_sem_analyzer(root);
    test_asm(fcodes);
    */
    delete root;
    return 0;
}

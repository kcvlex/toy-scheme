#include "parser.hpp"
#include "cps.hpp"
#include "closure.hpp"

std::string read_file(const std::string &filename) {
    std::ifstream ifs(filename);
    std::string code;
    code.assign(std::istreambuf_iterator<char>(ifs),
                std::istreambuf_iterator<char>());
    ifs.close();
    return code;
}

compiler::ASTNode* parse(const std::string &s) {
    compiler::TokenStream ts = std::move(compiler::Tokenizer::build(s));
    compiler::Parser parser(std::move(ts));
    return parser.parse();
}

compiler::CPSNode* to_cps(const compiler::ASTNode* const root) {
    compiler::AST2CPS visitor;
    root->accept(visitor);
    return visitor.res;
}

compiler::CPSNode* to_clsr(compiler::CPSNode* const root) {
    compiler::closure_translation(root);
    return root;
}

void test_translate(const std::string &filename) {
    auto root = to_clsr(to_cps(parse(read_file(filename))));
    compiler::print_cps_code("tmp.test.scm", root);
}

int main(int argc, char *argv[]) {
    test_translate(argv[1]);
}

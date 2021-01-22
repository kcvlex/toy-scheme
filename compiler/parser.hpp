#ifndef INCLUDE_PARSER
#define INCLUDE_PARSER

#include "token.hpp"
#include "ast.hpp"

namespace compiler {

struct Parser {
    Parser(TokenStream stream_arg);

    ASTNode* parse();

private:
    EvalNode*     parse_eval();
    LambdaNode*   parse_lambda();
    SymbolNode*   parse_symbol();
    ConstantNode* parse_constant();
    SequenceNode* parse_seq();
    BindNode*     parse_bind();
    ASTNode*      parse_expr();

    bool is_lambda() const noexcept;
    bool is_begin() const noexcept;
    bool is_define() const noexcept;
    bool lookup1(const std::string &s) const noexcept;

    TokenStream stream;
};

}

#endif
